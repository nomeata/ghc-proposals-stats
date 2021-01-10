{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TransformListComp #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Array as A
import qualified Data.Csv as CSV
import System.Directory
import Data.Traversable
import Data.List
import Data.Functor
import Data.Maybe
import Data.List.Split
import Control.Monad
import Data.Coerce
import Data.Monoid
import Text.Printf
import Data.Ord
import Text.Regex.TDFA
import System.FilePath
import Numeric.Statistics.PCA
import Numeric.LinearAlgebra.Data (toLists)

-- A record, intented to be loaded into GHC with record punning
type Ext = String
data D = D
  -- Totals
  { hackage_parsed   :: Int
  , hackage_in_cabal_total :: Int
  , survey_total :: Int
  , votes_total :: Int
  , exts :: [E]
  , categories :: [String]
  , ballots :: M.Map String (S.Set Ext)
  } deriving Show

data E = E
  { ext :: Ext              -- ^ The extension (redundant, as it's the map key, but convenient)
  , since :: String         -- ^ First GHC version
  , file :: String          -- ^ Documentation path
  , category :: String      -- ^ Category
  , hackage_used     :: Int -- ^ Used somewhere in the package
  , hackage_in_cabal :: Int -- ^ Turned on by default
  , hackage_mod_use  :: Int -- ^ Turnd on in modules, when the package uses default-extensions
  , survey_yes       :: Int -- ^ Survey votes in favor
  , survey_no        :: Int -- ^ Survey votes against
  , votes            :: Int -- ^ Committee votes
  } deriving Show

loadD :: IO D
loadD = do
  versions <- M.mapKeysWith (error "dup") norm . M.fromListWith (error "dup") . map (\(a,b,c,d) -> (a,(b,c,d))) <$> d "versions.csv"
  hackage_totals <- M.fromListWith (error "dup") <$> d "hackage-totals.csv"
  category_order <- map (\[x] -> x) <$> d "order.csv"
  hackage <- M.mapKeysWith (error "dup") norm . M.fromListWith (error "dup") . map (\(a,b,c,d) -> (a,(b,c,d))) <$> d "hackage-data.csv"
  (survey_total, survey) <- fromSurvey <$> d' "haskell-survey-results.csv"

  let hackage_parsed = hackage_totals M.! "parsed"
  let hackage_in_cabal_total = hackage_totals M.! "in-cabal"

  ballots <- M.fromList <$> readBallots
  let votes_total = length ballots
  let ballot_map = M.unionsWith (+) [ M.fromSet (const 1) s | s <- M.elems ballots ]
  putStrLn "Ignoring hackage data about unknown extensions:"
  putStrLn "(But compare with users_guide/expected-undocumented-flags.txt)"
  forM_ (M.toList (M.difference hackage versions)) $ \(ext, dat) ->
    printf "    %s (%s)\n"  ext (show dat)

  putStrLn "Ignoring survey data about unknown extensions:"
  forM_ (M.toList (M.difference survey versions)) $ \(ext, dat) ->
    printf "    %s (%s)\n"  ext (show dat)

  putStrLn "Ignoring votes about unknown extensions:"
  forM_ (M.toList (M.difference ballot_map versions)) $ \(ext, dat) ->
    printf "    %s (%s)\n"  ext (show dat)

  let exts = M.toList versions <&> \(ext, (since, file, category)) ->
        let (hackage_used, hackage_in_cabal, hackage_mod_use) = M.findWithDefault (0,0,0) ext hackage in
        let (survey_yes, survey_no) = M.findWithDefault (0,0) ext survey in
        let votes = M.findWithDefault 0 ext ballot_map in
        E{..}

  let categories = nub $ category_order ++ [ cat | (_, _, cat) <- M.elems versions ]

  return D{..}

readBallots :: IO [(String, S.Set String)]
readBallots = do
    voters <- mapMaybe (stripSuffix ".txt") <$> listDirectory "GHC2021/votes"
    printf "%d votes found (%s)\n" (length voters) (intercalate ", " voters)
    for voters $ \voter -> do
        let filename = "GHC2021/votes/" ++ voter ++ ".txt"
        s <- readFile filename
        let exts = [ ext | [_, ext] <- s =~ "^([A-Z][A-Za-z0-9]+): yes" ]
        when (null exts) $
            printf "WARNING: No votes found in %s\n" filename
        return (voter, S.fromList exts)
   where stripSuffix p = fmap reverse . stripPrefix (reverse p) . reverse

toRst :: D -> String
toRst D{..} = unlines $
    [ printf "Executive summary"
    , printf "-----------------"
    , ""
    ] ++
    [ unlines $
      [ printf "* %s" cat , "" ] ++
      [ printf "  * Safely in: %s"  (commas safely_in) | not (null safely_in)] ++
      [ printf "  * Barely in: %s"  (commas barely_in) | not (null barely_in)] ++
      [ printf "  * Barely out: %s" (commas barely_out) | not (null barely_out)]
    | cat <- categories
    , let safely_in =
            [ rstAnchor ext
            | E{..} <- exts
            , category == cat , votes >= quorum + 1
            , then sortOn by Down votes
            ]
    , let barely_in =
            [ rstAnchor ext | E{..} <- exts, category == cat, votes == quorum ]
    , let barely_out =
            [ rstAnchor ext | E{..} <- exts, category == cat, votes == quorum - 1 ]
    , not (null safely_in && null barely_in && null barely_out)
    ] ++
    [ printf "Full Results"
    , printf "------------"
    , ""
    , printf "Data based on %d hackage packages, %d survey responses and %d committee votes. (Votes may be changed. Bold votes are currently above 2/3.)" hackage_parsed survey_total votes_total
    , ""
    ] ++
    [ rstTable header
          [ [ rstAnchor ext
            , printVotes votes votes_total ] ++
            [ if ext `S.member` s then "✔" else "" | s <- M.elems ballots ] ++
            [ since
            , survey_yes `outOf` survey_total, survey_no `outOf` survey_yes
            , hackage_used `outOf` hackage_parsed
            , hackage_in_cabal `outOf` hackage_in_cabal_total
            , hackage_mod_use `outOf` hackage_in_cabal_total
            ]
          | E{..} <- exts, then sortOn by Down votes
          ]
    ] ++
    [ ".. _" ++ ext ++ ": " ++ extHref ext file | E{..} <- exts ]
  where
    header = [ "Extension"
             , "Votes" ] ++
             M.keys ballots ++
             [ "Since"
             , "Pop…", "Cont…"
             , "Prolif…", "Innoc…", "Aloof…"
             ]
    _rstLink txt url = "`" ++ txt ++ " <" ++ url ++ ">`_"
    rstAnchor txt = "`" ++ txt ++ "`_"
    extHref ext file =
        printf "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/%s.html#extension-%s"
            (dropExtension file) ext
    commas = intercalate ", "
    quorum = ceiling (fromIntegral votes_total * 2/3 :: Double)

sadnessReport :: D -> String
sadnessReport D{..} = unlines $ flip foldMap (M.toList ballots) $ \(n, b) ->
    [ n
    , printf "would miss:"
    , pp (b `S.difference` accepted)
    , "doesn’t want:"
    , pp (accepted `S.difference` b)
    , ""
    ]
  where
    accepted = S.fromList [ ext | E{..} <- exts, 3 * votes >= 2 * votes_total ]
    pp s | S.null s = "none!"
    pp s = intercalate ", " $ S.toList s

main :: IO ()
main = do
  d <- loadD
  writeFile "GHC2021/result.rst" $ toRst d

printVotes :: Int -> Int -> String
printVotes votes total = printf formatString votes
 where formatString
        | 3 * votes >= 2 * total = "**%d**"
        | otherwise              = "%d"

outOf :: Int -> Int -> String
outOf 0 0 = "N/A"
outOf _ 0 = "∞%"
outOf 0 _ = "0"
outOf x y = printf "%.0f%%" (fromIntegral x / fromIntegral y * 100 :: Double)


norm :: Ext -> Ext
norm "GeneralizedNewtypeDeriving" = "GeneralisedNewtypeDeriving"
norm s = s

-- would have use row-types if the ideas in
-- https://www.danwc.com/posts/2020-06-01-row-types-for-csv-library/
-- were a readily available library

d :: CSV.FromRecord a => String -> IO [a]
d file =
    V.toList .
    either error id .
    CSV.decodeWith CSV.defaultDecodeOptions CSV.HasHeader <$>
    BS.readFile ("GHC2021/" ++ file)
d' :: CSV.FromNamedRecord a => String -> IO [a]
d' file =
    V.toList .
    either error snd .
    CSV.decodeByName <$>
    BS.readFile ("GHC2021/" ++ file)

rstTable :: [String] -> [[String]] -> String
rstTable header rows = unlines $
    headerWithSep ++
    intercalate innerHeader (chunksOf 20 (map padRow rows)) ++
    [ sep ]
  where
    headerWithSep = [ sep, padRow header, sep ]
    innerHeader = [ padRow boldHeader ]
    boldHeader = map (printf "**%s**") header
    widths = map (maximum . map length) (transpose (boldHeader : rows))
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

distanceGraph :: D -> String
distanceGraph D{..} = unlines  $
    [ "graph {" ] ++
    [ "graph [mode=KK, maxiter=100000, splines=true];" ] ++
    [ printf "  %s;" k | k <- M.keys ballots ] ++
    [ printf "  %s -- %s [len=%.2f, label=\"%2.0f%%\"];" k1 k2 ((1-d) * 20) (d * 100)
    | k1 <- M.keys ballots, k2 <- M.keys ballots , k1 < k2, let d = dist k1 k2 ] ++
    [ "}" ]
  where
    dist x y =
      fromIntegral (length [ () | E{ext} <- exts,  ext `S.member` (ballots M.! x) == ext `S.member` (ballots M.! y)]) / fromIntegral (length exts) :: Double

pcaAnalysis :: D -> IO ()
pcaAnalysis D{..} = do
    writeFile "pca.csv" pcs_csv
    writeFile "vectors.csv" vectors
  where
    interestingExts =
        [ (ext, category)
        | E{ext, category} <- exts
        , any (ext `S.member`) (M.elems ballots)
        , any (ext `S.notMember`) (M.elems ballots)
        ]

    input :: A.Array Int (VS.Vector Double)
    input = A.listArray (0,M.size ballots-1) [ VS.fromList [ if ext `S.member` b then 1.0 else 0.0 | (ext, _) <- interestingExts ] | b <- M.elems ballots ]

    (_evs, pcs) = pcaN input 2
    pcs_csv = unlines $
         [ "name,x,y" ] ++
         [ printf "%s,%.3f,%.3f" e x y | (e, [x,y]) <- zip (M.keys ballots) (toLists pcs)]
    pca_trans = pcaTransform input pcs
    vectors = unlines $
         [ "ext,cat,x,y" ] ++
         [ printf "%s,%s,%.3f,%.3f" ext cat x y
         | ((ext,cat),x,y) <- zip3
            interestingExts
            (VS.toList (pca_trans A.! 1))
            (VS.toList (pca_trans A.! 2))
         ]

-- normalize names
surveyTick :: Ext -> Bool -> M.Map Ext (Sum Int, Sum Int)
surveyTick "Cpp"                        v = surveyTick' "CPP"                        v
surveyTick "GeneralizedNewtypeDeriving" v = surveyTick' "GeneralisedNewtypeDeriving" v
surveyTick "Rank2Types"                 v = surveyTick' "RankNTypes"                 v
-- flip negatives
surveyTick "NoEmptyDataDecls"           v = surveyTick' "EmptyDataDecls"             (not v)
surveyTick "NoMonadFailDesugaring"      v = surveyTick' "MonadFailDesugaring"        (not v)
surveyTick ext v = surveyTick' ext v

surveyTick' :: Ext -> Bool -> M.Map Ext (Sum Int, Sum Int)
surveyTick' ext True  = M.singleton ext (Sum (1::Int), mempty)
surveyTick' ext False = M.singleton ext (mempty, Sum (1::Int))
