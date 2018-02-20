{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Network.Wreq
import Data.Monoid
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Map as M
import Data.Aeson.Types
import Debug.Trace
import Text.Layout.Table
import Data.List
import Data.Foldable
import Data.Traversable
import Data.Bifunctor
import Data.Maybe
import Text.Printf
import GHC.Generics
import Data.Time
import System.Environment

import Sankey

type IssueNumber = Integer

data IssueSummary = IssueSummary
    { number :: IssueNumber
    , assignee :: Maybe Text
    , status :: Maybe Text
    , created :: UTCTime
    , isPullRequest :: Bool
    }
  deriving (Generic, Show)

instance ToJSON IssueSummary where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON IssueSummary

data State = State
    { issueSummaries :: [IssueSummary]
    , histories :: [(Timed StateChange, IssueNumber)]
    }
    deriving (Generic, Show)

instance ToJSON State where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON State


summarizeIssue :: Value -> IssueSummary
summarizeIssue v =
    -- traceShow v $
    IssueSummary {..}
  where
    assignee = v ^? key "assignees" . nth 0 . key "login" . _String
    status = v ^? key "labels" . nth 0 . key "name" ._String
    Just number = v ^? key "number" . _Integer
    Just created = v ^? key "created_at" . _JSON
    isPullRequest = isJust $ v ^? key "pull_request"


histogram2 :: (a -> Maybe Text) -> (a -> Maybe Text) -> [a] -> String
histogram2 getX getY dat = tableString
    (def : (numCol <$ xs) ++ [numCol])
    unicodeS
    (titlesH ("" : map pr xs ++ ["∑"])) $
    [ rowG $ pr y : [ show (count x y) | x <- xs] ++ [ show (countY y) ] | y <- ys ] ++
    [ rowG $ "∑"  : [ show (countX x) | x <- xs] ++ [ show (length dat)] ]
  where
    xs = Nothing : map Just (sort (nub (mapMaybe getX dat)))
    ys = Nothing : map Just (sort (nub (mapMaybe getY dat)))

    pr (Just t) | "Pending" `T.isPrefixOf` t = "Pending"
                | otherwise = T.unpack t
    pr Nothing = "-"

    count x y = length [ () | v <- dat, getX v == x, getY v == y ]
    countX x  = length [ () | v <- dat, getX v == x ]
    countY y  = length [ () | v <- dat, getY v == y ]

type PullState = Maybe Text

type Timed a = (a, NominalDiffTime)

type StateChange = (PullState, PullState)

stateChanges :: UTCTime -> IssueSummary -> [Value] -> [Timed StateChange]
stateChanges now issue evs = prune $ go (created issue) "discussed" evs
  where
    -- Record new labels
    go :: UTCTime -> Text -> [Value] -> [Timed StateChange]
    go since state (e:es)
        | Just "labeled" <- e ^? key "event" . _String
        , Just l <- e ^? key "label" . key "name" . _String
        , Just d <- e ^? key "created_at" . _JSON
        , let l' = if l == "Under discussion" then "discussed"
                                              else l
        = let sc = ((Just state, Just l'), d `diffUTCTime` since)
          in sc : go d l' es
    -- Record removals of the current label (but ignore other removals)
    go since state (e:es)
        | Just "unlabeled" <- e ^? key "event" . _String
        , Just l <- e ^? key "label" . key "name" . _String
        , Just d <- e ^? key "created_at" . _JSON
        , l == state
        = let sc = ((Just state, Just "discussed"), d `diffUTCTime` since)
          in sc : go d "discussed" es
    go since state (_:es) = go since state es
    go since state [] = [((Just state, Nothing), now `diffUTCTime` since)]

    prune (((s1, s2), d1) : ((s3, s4), d2) : scs)
        | s2 == s3
        , s1 == s2 -- not really a state change
        = prune (((s1, s4), d1 + d2) : scs)
    prune (((s1, s2), d1) : ((s3, s4), d2) : scs)
        | s2 == s3 -- everything else would be weird
        , d1 < 3600
        = prune (((s1, s4), d1 + d2) : scs)
    prune (sc:scs) = sc : prune scs
    prune [] = []

timedHistogram :: Ord a => [(Timed a,b)] -> [(a,Integer,NominalDiffTime, [b])]
timedHistogram = map norm . M.toList . M.unionsWith mappend . map prep
  where
    prep ((a,d),b) = M.singleton a (Sum d, Sum 1, [b])
    norm (a,(Sum d, Sum c, bs)) = (a, c, d / fromIntegral c, bs)

getState :: IO State
getState = do
    args <- getArgs
    if args == ["--update"] then do
        tok <- BS.readFile "github-token.txt"
        let opts = defaults & auth ?~ oauth2Token tok
        r1 <- asValue =<< getWith opts "https://api.github.com/repos/ghc-proposals/ghc-proposals/issues?per_page=100&state=all"
        r2 <- asValue =<< getWith opts "https://api.github.com/repos/ghc-proposals/ghc-proposals/issues?per_page=100&state=all&page=2"
        let summaries = filter isPullRequest $ fmap summarizeIssue $ toList $
                (r1 ^. responseBody . _Array) <> (r2 ^. responseBody . _Array)

        now <- getCurrentTime
        histories <- forM summaries $ \is -> do
            r <- asValue =<< getWith opts (printf "https://api.github.com/repos/ghc-proposals/ghc-proposals/issues/%d/events?per_page=100" (number is))
            let hist = toList (r ^. responseBody . _Array)
            return $ map (,number is) $ stateChanges now is hist
        let allStateChanges = concat histories
        let s = State summaries allStateChanges
        BSL.writeFile "cache.json" (encode s)
        return s
    else
        fromJust . decode <$> BSL.readFile "cache.json"

main = do
    State summaries stateChanges <- getState

    putStrLn $ histogram2 assignee status summaries
    putStrLn ""

    let stateHist = timedHistogram stateChanges
    putStrLn $ stateChangeTable stateHist

    BSL.writeFile "sankey.json" $ encode $ mkCsaldenSankey
        [ (prState from, c) | ((from, Nothing), c, t, ns) <- stateHist]
        [ (prState from, prState to, c) | ((from, to@(Just _)), c, t, ns) <- stateHist]

    BS.writeFile "sankey.html" =<< mkSankeyHTML
        [ (prState from, c) | ((from, Nothing), c, t, ns) <- stateHist]
        [ (prState from, prState to, c) | ((from, to@(Just _)), c, t, ns) <- stateHist]

stateChangeTable :: [(StateChange, Integer, NominalDiffTime, [IssueNumber])] -> String
stateChangeTable dat = tableString
    [def, def, numCol, numCol, def]
    unicodeS
    (titlesH ["From", "To", "Count", "Avg. time", "Issues"])
    [ rowG $ [prState from, prState to, show c, showT t, showNums ns] | ((from, to), c, t, ns) <- dat]
  where
    showT t = show (round (t / (3600*24))) ++ " days"
    showNums ns = (intercalate ", " $ map ('#':) $ map show start) ++ more
      where
        start = take 8 ns
        more | length ns > 8 = "…"
             | otherwise = ""


prState (Just t) | "Pending" `T.isPrefixOf` t = "Pending"
                 | otherwise = T.unpack t
prState Nothing = "-"
