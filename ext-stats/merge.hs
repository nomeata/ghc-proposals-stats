{-# LANGUAGE RecordWildCards #-}
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Csv as CSV
import Data.List
import Data.List.Split
import Data.Coerce
import Data.Monoid
import Text.Printf
import Data.Char
import Data.Functor
import Data.Foldable

-- A record, intented to be loaded into GHC with record punning
data D = D
  -- Totals
  { hackage_parsed   :: Int
  , hackage_in_cabal_total :: Int
  , survey_total :: Int
  , exts :: M.Map String E
  } deriving Show

data E = E
  { ext :: String -- ^ The extension (redundant, as it's the map key, but convenient)
  , since :: String         -- ^ First GHC version
  , hackage_used     :: Int -- ^ Used somewhere in the package
  , hackage_in_cabal :: Int -- ^ Turned on by default
  , hackage_mod_use  :: Int -- ^ Turnd on in modules, when the package uses default-extensions
  , survey_yes       :: Int -- ^ Survey votes in favor
  , survey_no        :: Int -- ^ Survey votes against
  } deriving Show

loadD :: IO D
loadD = do
  versions <- M.fromListWith (error "dup") <$> d "versions.csv"
  hackage_totals <- M.fromListWith (error "dup") <$> d "hackage-totals.csv"
  hackage <- M.mapKeysWith (error "dup") norm . M.fromListWith (error "dup") . map (\(a,b,c,d) -> (a,(b,c,d))) <$> d "hackage-data.csv"
  (survey_total, survey) <- fromSurvey <$> d' "haskell-survey-results.csv"

  let hackage_parsed = hackage_totals M.! "parsed"
  let hackage_in_cabal_total = hackage_totals M.! "in-cabal"

  putStrLn "Ignoring hackage data about unknown extensions:"
  putStrLn "(But compare with users_guide/expected-undocumented-flags.txt)"
  forM_ (M.toList (M.difference hackage versions)) $ \(ext, dat) ->
    printf "    %s (%s)\n"  ext (show dat)

  putStrLn "Ignoring survey data about unknown extensions:"
  forM_ (M.toList (M.difference survey versions)) $ \(ext, dat) ->
    printf "    %s (%s)\n"  ext (show dat)

  let exts = (`M.mapWithKey` versions) $ \ext since ->
        let (hackage_used, hackage_in_cabal, hackage_mod_use) = M.findWithDefault (0,0,0) ext hackage in
        let (survey_yes, survey_no) = M.findWithDefault (0,0) ext survey in
        E{..}
  return D{..}

toRst :: D -> String
toRst D{..} = unlines $
    [ rstTable header
          [ [ rstAnchor ext
            , since
            , hackage_used `outOf` hackage_parsed
            , hackage_in_cabal `outOf` hackage_in_cabal_total
            , hackage_mod_use `outOf` hackage_in_cabal_total
            , survey_yes `outOf` survey_total, survey_no `outOf` survey_yes
            , "N/A"
            ]
          | E{..} <- M.elems exts
          ]
    ] ++
    [ ".. _" ++ ext ++ ": " ++ extHref ext | E{..} <- M.elems exts ]
  where
    header = [ "Extension", "In GHC Since"
             , "Proliferation", "Innocuousness", "Aloofness"
             , "Popularity", "Contentionsness"
             , "Votes"
             ]
    rstLink txt url = "`" ++ txt ++ " <" ++ url ++ ">`_"
    rstAnchor txt = "`" ++ txt ++ "`_"
    extHref ext = "https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-" ++ ext

main = do
  d <- loadD
  writeFile "GHC2021/result.rst" $ toRst d

outOf :: Int -> Int -> String
outOf 0 0 = "N/A"
outOf x 0 = "∞%"
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
    CSV.decodeWith CSV.defaultDecodeOptions CSV.HasHeader <$>
    BS.readFile ("GHC2021/" ++ file)
d' file =
    V.toList .
    either error snd .
    CSV.decodeByName <$>
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
csvTable header rows = unlines $ map (intercalate ",") (header : rows)



fromSurvey :: [M.Map String String] -> (Int, M.Map String (Int, Int))
fromSurvey survey =
    ( length survey
    , coerce $ M.unionsWith mappend $ map (\r ->
      M.unionsWith mappend [
          case vote of ('+': ext) -> surveyTick ext True
                       ('-': ext) -> surveyTick ext False
                       _ -> error $ "Unexpected vote: " ++ vote ++ " in " ++ show (r M.! "s2q5")
          | vote <-  split (dropBlanks (dropDelims (oneOf ","))) (r M.! "s2q5") ]
      ) survey
    )

-- normalize names
surveyTick "Cpp"                        v = surveyTick' "CPP"                        v
surveyTick "GeneralizedNewtypeDeriving" v = surveyTick' "GeneralisedNewtypeDeriving" v
surveyTick "Rank2Types"                 v = surveyTick' "RankNTypes"                 v
-- flip negatives
surveyTick "NoEmptyDataDecls"           v = surveyTick' "EmptyDataDecls"             (not v)
surveyTick "NoMonadFailDesugaring"      v = surveyTick' "MonadFailDesugaring"        (not v)
surveyTick ext v = surveyTick' ext v

surveyTick' ext True  = M.singleton ext (Sum (1::Int), mempty)
surveyTick' ext False = M.singleton ext (mempty, Sum (1::Int))
