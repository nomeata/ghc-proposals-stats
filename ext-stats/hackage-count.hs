import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Control.Monad
import Control.Exception
import System.FilePath
import System.Environment
import System.Exit
import System.IO
import System.Directory
import Text.Printf

import Extensions


main = do
  args <- getArgs
  sets <- forM args $ \dir -> do
    cabalPath <- findCabalFile dir
    -- change cwd due to https://github.com/kowainik/extensions/issues/69
    previous <- getCurrentDirectory
    setCurrentDirectory (takeDirectory cabalPath)
    cabalExts <- handleCabalException $ getPackageExtentions (takeFileName cabalPath)
    setCurrentDirectory previous
    case mergeExtensions . concatMap (Set.toList . extensionsAll) <$> sequence (Map.elems cabalExts) of
        Left err   -> do
            hPutStrLn stderr $ "Failed to parse files in " ++ dir ++ ":"
            hPrint stderr err
            return Nothing
        Right exts ->
            return (Just exts)
  let total = length sets
  let failures = length [ () | Nothing <- sets ]
  let parsed = total - failures
  let tally =
        sortOn snd $ Map.toList $
        Map.unionsWith (+) [ Map.fromSet (const 1) s | Just s <- sets ]

  forM_ tally $ \(e, n) -> printf "%4d %s\n" (n::Int) (showOnOffExtension e)
  printf "--------------------\n"
  printf "%4d packages total\n" total
  printf "%4d packages parsed\n" parsed


findCabalFile :: FilePath -> IO FilePath
findCabalFile dirPath = do
    dirContent <- listDirectory dirPath
    case filter (\p -> takeExtension p == ".cabal") dirContent of
        [] -> exitAfter $ hPutStrLn stderr $ ".cabal file not found in " ++ dirPath
        [cabalFile] ->
            pure $ dirPath </> cabalFile
        l -> exitAfter $ hPutStrLn stderr $
            "Multiple .cabal files found in " ++ dirPath ++ ": " <>
            intercalate ", " l

exitAfter :: IO a -> IO b
exitAfter action = action >> exitFailure

handleCabalException :: IO a -> IO a
handleCabalException action = action `catch` \exc -> exitAfter $
    hPutStrLn stderr $ "Error processing .cabal file: " <> show (exc :: CabalException)

