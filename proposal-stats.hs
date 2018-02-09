{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Network.Wreq
import Data.Monoid
import Control.Lens
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
import Data.Time

opts = defaults & auth ?~ oauth2Token "2628d42f735b72707c3de5ded7f1c1006dc8cb0b"

data IssueSummary = IssueSummary
    { number :: Integer
    , assignee :: Maybe Text
    , status :: Maybe Text
    , created :: UTCTime
    , isPullRequest :: Bool
    }
  deriving Show

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
    (titlesH ("" : map pr xs ++ ["total"])) $
    [ rowG $ pr y : [ show (count x y) | x <- xs] ++ [ show (countY y) ] | y <- ys ] ++
    [ rowG $ "total"  : [ show (countX x) | x <- xs] ++ [ show (length dat)] ]
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

data StateChange = StateChange
        { fromState :: PullState
        , toState   :: PullState
        }
  deriving (Show, Eq, Ord)

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
        = let sc = (StateChange (Just state) (Just l'), d `diffUTCTime` since)
          in sc : go d l' es
    -- Record removals of the current label (but ignore other removals)
    go since state (e:es)
        | Just "unlabeled" <- e ^? key "event" . _String
        , Just l <- e ^? key "label" . key "name" . _String
        , Just d <- e ^? key "created_at" . _JSON
        , l == state
        = let sc = (StateChange (Just state) (Just "discussed"), d `diffUTCTime` since)
          in sc : go d "discussed" es
    go since state (_:es) = go since state es
    go since state [] = [(StateChange (Just state) Nothing, now `diffUTCTime` since)]

    prune ((StateChange s1 s2, d1) : (StateChange s3 s4, d2) : scs)
        | s2 == s3
        , s1 == s2 -- not really a state change
        = prune ((StateChange s1 s4, d1 + d2) : scs)
    prune ((StateChange s1 s2, d1) : (StateChange s3 s4, d2) : scs)
        | s2 == s3 -- everything else would be weird
        , d1 < 3600
        = prune ((StateChange s1 s4, d1 + d2) : scs)
    prune (sc:scs) = sc : prune scs
    prune [] = []

timedHistogram :: Ord a => [Timed a] -> [(a,Integer,NominalDiffTime)]
timedHistogram = map norm . M.toList . M.unionsWith mappend . map prep
  where
    prep (a,d) = M.singleton a (Sum d, Sum 1)
    norm (a,(Sum d, Sum c)) = (a, c, d / fromIntegral c)

main = do
    tok <- BS.readFile "github-token.txt"
    let opts = defaults & auth ?~ oauth2Token tok
    r1 <- asValue =<< getWith opts "https://api.github.com/repos/ghc-proposals/ghc-proposals/issues?per_page=100&state=all"
    r2 <- asValue =<< getWith opts "https://api.github.com/repos/ghc-proposals/ghc-proposals/issues?per_page=100&state=all&page=2"
    let summaries = filter isPullRequest $ fmap summarizeIssue $ toList $
            (r1 ^. responseBody . _Array) <> (r2 ^. responseBody . _Array)

    putStrLn $ histogram2 assignee status summaries
    putStrLn ""

    now <- getCurrentTime

    histories <- forM summaries $ \is -> do
        r <- asValue =<< getWith opts (printf "https://api.github.com/repos/ghc-proposals/ghc-proposals/issues/%d/events?per_page=100" (number is))
        let hist = toList (r ^. responseBody . _Array)
        return $ stateChanges now is hist
    let allStateChanges = concat histories

    let stateHist = timedHistogram allStateChanges
    putStrLn $ stateChangeTable stateHist

stateChangeTable :: [(StateChange, Integer, NominalDiffTime)] -> String
stateChangeTable dat = tableString
    [def, def, numCol, numCol]
    unicodeS
    (titlesH ["From", "To", "Count", "Avg. time"])
    [ rowG $ [prState from, prState to, show c, showT t] | (StateChange from to, c, t) <- dat]
  where
    showT t = show (round (t / (3600*24))) ++ " days"


prState (Just t) | "Pending" `T.isPrefixOf` t = "Pending"
                 | otherwise = T.unpack t
prState Nothing = "-"
