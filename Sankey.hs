{-# LANGUAGE OverloadedStrings #-}

module Sankey where

import Data.Aeson
import Data.List
import Data.Monoid
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

-- Data for https://sankey.csaladen.es/

mkCsaldenSankey :: [(String, Integer)] -> [(String, String, Integer)] -> Value
mkCsaldenSankey values dat = object
    [ "sankey" .= mkSankey values dat
    , "params" .= params
    ]
  where
    params = [0.5,0.25,0,0] :: [Double]

mkSankey :: [(String, Integer)] -> [(String, String, Integer)] -> Value
mkSankey values dat = object
        [ "nodes" .= nodes
        , "links" .= links
        ]
  where
    nodeNames = nub $ map fst values ++ concat [[f,t] | (f,t,_) <- dat ]
    idx = (M.fromList (zip nodeNames [0::Integer ..]) M.!)
    valMap = M.fromList values
    nodes =
        [ object [ "name" .= n, "value" .= M.lookup n valMap ]
        | n <- nodeNames
        ]
    links =
        [ object [ "source" .= idx f, "target" .= idx t, "value" .= i]
        | (f,t,i) <- dat
        ]

mkSankeyHTML :: [(String, Integer)] -> [(String, String, Integer)] -> IO BS.ByteString
mkSankeyHTML values dat = do
    tmpl <- BS.readFile "sankey-template.html"
    let json = BSL.toStrict $ encode $ mkSankey values dat
    let pat = "var data = {};"
    let (before, after) = BS.breakSubstring pat tmpl
    return $ before <> "var data = " <> json <> ";" <> BS.drop (BS.length pat) after
