{-# LANGUAGE OverloadedStrings #-}

module Sankey where

import Data.Aeson
import Data.List
import qualified Data.Map as M

-- Data for https://sankey.csaladen.es/

mkSankey :: [(String, Integer)] -> [(String, String, Integer)] -> Value
mkSankey values dat = object
    [ "sankey" .= object
        [ "nodes" .= nodes
        , "links" .= links
        ]
    , "params" .= params
    ]
  where
    nodeNames = nub $ map fst values ++ concat [[f,t] | (f,t,_) <- dat ]
    idx = (M.fromList (zip nodeNames [0::Integer ..]) M.!)
    valMap = M.fromList values
    params = [0.5,0.25,0,0] :: [Double]
    nodes =
        [ object [ "name" .= n, "value" .= M.lookup n valMap ]
        | n <- nodeNames
        ]
    links =
        [ object [ "source" .= idx f, "target" .= idx t, "value" .= i]
        | (f,t,i) <- dat
        ]
