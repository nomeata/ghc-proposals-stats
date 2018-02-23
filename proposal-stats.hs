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
    , updated :: UTCTime
    , closed :: Bool
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


getNumber :: Value -> Integer
getNumber v = number
  where
    Just number = v ^? key "number" . _Integer

summarizeIssue :: Value -> IssueSummary
summarizeIssue v =
    -- traceShow v $
    IssueSummary {..}
  where
    assignee = v ^? key "assignees" . nth 0 . key "login" . _String
    status = v ^? key "labels" . nth 0 . key "name" ._String
    Just number = v ^? key "number" . _Integer
    Just created = v ^? key "created_at" . _JSON
    Just updated = v ^? key "updated_at" . _JSON
    isPullRequest = isJust $ v ^? key "pull_request"
    closed = isJust $ (v ^? key "closed_at" . _String :: Maybe Text)


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

type RawState = [RawIssue]
data RawIssue = RawIssue
    { issueJson :: Value
    , issueEvents :: [Value]
    }
    deriving (Generic, Show)

instance ToJSON RawIssue where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON RawIssue

getFreshState :: IO RawState
getFreshState = do
    tok <- BS.readFile "github-token.txt"
    let opts = defaults & auth ?~ oauth2Token tok
    r1 <- asValue =<< getWith opts "https://api.github.com/repos/ghc-proposals/ghc-proposals/issues?per_page=100&state=all"
    r2 <- asValue =<< getWith opts "https://api.github.com/repos/ghc-proposals/ghc-proposals/issues?per_page=100&state=all&page=2"
    forM (toList $ (r1 ^. responseBody . _Array) <> (r2 ^. responseBody . _Array)) $ \issueJson -> do
        let n = getNumber issueJson
        r <- asValue =<< getWith opts (printf "https://api.github.com/repos/ghc-proposals/ghc-proposals/issues/%d/events?per_page=100" n)
        let issueEvents = toList $ r ^. responseBody . _Array
        return $ RawIssue {..}


getState :: UTCTime -> IO State
getState now = do
    args <- getArgs
    rawState <-
        if args == ["--update"] then do
            rawState <- getFreshState
            BSL.writeFile "cache.json" (encode rawState)
            return rawState
        else
            fromJust . decode <$> BSL.readFile "cache.json"

    let stuff = [ (is, map (,number is) $ stateChanges now is hist)
                | RawIssue ij hist <- rawState
                , let is = summarizeIssue ij
                , isPullRequest is
                ]

    let summaries = map fst stuff
    let allStateChanges = concatMap snd stuff
    return $ State summaries allStateChanges

main = do
    now <- getCurrentTime
    State summaries stateChanges <- getState now

    putStrLn $ histogram2 assignee status summaries
    putStrLn ""

    let stateHist = timedHistogram stateChanges
    putStrLn $ stateChangeTable stateHist
    putStrLn ""

    putStrLn $ shouldBeDormant now summaries

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
    [ rowG $ [prState from, prState to, show c, showTimeDiff t, showNums ns]
    | ((from, to), c, t, ns) <- dat]

showNums :: [Integer] -> String
showNums ns = (intercalate ", " $ map ('#':) $ map show start) ++ more
  where
    start = take 8 ns
    more | length ns > 8 = "…"
         | otherwise = ""

showNum :: Integer -> String
showNum = printf "https://github.com/ghc-proposals/ghc-proposals/pull/%d"

showTimeDiff :: NominalDiffTime -> String
showTimeDiff t = show (round (t / (3600*24))) ++ " days"

shouldBeDormant :: UTCTime -> [IssueSummary] -> String
shouldBeDormant now iss = tableString
    [def, def, numCol, numCol, def]
    unicodeS
    (titlesH ["Last activity", "State", "Issue"])
    [ rowG $ [showTimeDiff age, prState (status is), showNum (number is)]
    | is <- iss'
    , let age = now `diffUTCTime` updated is
    , age > 30 * 24 * 3600
    , not (closed is)
    , status is `elem` [Nothing, Just "dormant", Just "Needs revision"]
    ]
  where
    iss' = sortOn updated iss

prState (Just t) | "Pending" `T.isPrefixOf` t = "Pending"
                 | otherwise = T.unpack t
prState Nothing = "-"
