import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Csv as CSV
import Data.List
import Text.Printf
import Data.Char
import Data.Functor
import Data.Foldable

main = do

  versions <- M.fromList <$> d "versions.csv"
  hackage_totals <- M.fromList <$> d "hackage-totals.csv"
  hackage <- M.mapKeys norm . M.fromList . map (\(a,b,c,d) -> (a,(b,c,d))) <$> d "hackage-data.csv"

  let parsed = hackage_totals M.! "parsed"
  let in_cabal = hackage_totals M.! "in-cabal"


  let header = [ "Extension", "In GHC Since"
               , "Proliferation", "Innocuousness", "Aloofness"
               , "Popularity", "Contentionsness"
               , "Votes"
               ]
  let rows = M.toList versions <&> \(ext, since) ->
        let (use, used_in_cabal, per_mod_use) = M.findWithDefault (0,0,0) ext hackage in
        [ ext, since
        , use `outOf` parsed, used_in_cabal `outOf` in_cabal, per_mod_use `outOf` in_cabal
        , "N/A", "N/A"
        , "N/A"
        ]

  putStrLn "Ignoring hackage data about unknown extensions:"
  putStrLn "(But compare with users_guide/expected-undocumented-flags.txt)"
  forM_ (M.toList (M.difference hackage versions)) $ \(ext, dat) ->
    printf "    %s (%s)\n"  ext (show dat)

  writeFile "GHC2021/result.rst" $ rstTable header rows
  writeFile "GHC2021/result.csv" $ csvTable header rows

outOf :: Int -> Int -> String
outOf x 0 = "---%"
outOf 0 _ = "0"
outOf x y = printf "%.0f%%" (fromIntegral x / fromIntegral y * 100 :: Double)


norm "GeneralizedNewtypeDeriving" = "GeneralisedNewtypeDeriving"
norm s = s

-- would have use row-types if the ideas in
-- https://www.danwc.com/posts/2020-06-01-row-types-for-csv-library/
-- were a readily available library

d file =
    V.toList .
    either error id .
    CSV.decodeWith
        CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord ';') }
        CSV.HasHeader <$>
    BS.readFile ("GHC2021/" ++ file)

rstTable :: [String] -> [[String]] -> String
rstTable header rows = unlines $
    [ sep, padRow header, sep ] ++ map padRow rows ++ [ sep ]
  where
    widths = map (maximum . map length) (transpose (header : rows))
    padRow = unwords . zipWith padField widths
    padField = printf "%*s"
    sep = unwords $ map (`replicate` '=') widths

csvTable :: [String] -> [[String]] -> String
csvTable header rows = unlines $ map (intercalate ";") (header : rows)
