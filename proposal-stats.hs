{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Network.Wreq
import Data.Monoid
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson.Types
import Debug.Trace
import Text.Layout.Table
import Data.List
import Data.Foldable
import Data.Maybe

opts = defaults & auth ?~ oauth2Token "2628d42f735b72707c3de5ded7f1c1006dc8cb0b"

data IssueSummary = IssueSummary
    { number :: Integer 
    , assignee :: Maybe Text
    , status :: Maybe Text
    }
  deriving Show

summarizeIssue :: Value -> IssueSummary
summarizeIssue v =
    -- traceShow v $
    IssueSummary {..}
  where
    assignee = v ^? key "assignees" . nth 0 . key "login" . _String
    status = v ^? key "labels" . nth 0 . key "name" ._String
    number = fromJust $ v ^? key "number" . _Integer


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


main = do
    tok <- BS.readFile "github-token.txt"
    let opts = defaults & auth ?~ oauth2Token tok
    r1 <- asValue =<< getWith opts "https://api.github.com/repos/ghc-proposals/ghc-proposals/issues?per_page=100&state=all"
    r2 <- asValue =<< getWith opts "https://api.github.com/repos/ghc-proposals/ghc-proposals/issues?per_page=100&state=all&page=2"
    let responses = (r1 ^. responseBody . _Array) <> (r2 ^. responseBody . _Array)

    let summaries = fmap summarizeIssue responses
    putStrLn $ histogram2 assignee status (toList summaries)
    -- print $ sort $ map number (toList summaries)

