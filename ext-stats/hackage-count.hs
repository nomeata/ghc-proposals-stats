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
import Data.Monoid
import Text.Printf
import Data.Containers.ListUtils

import Extensions


main = do
  args <- getArgs
  sets <- forM args $ \dir -> do
    cabalPath <- findCabalFile dir
    -- change cwd due to https://github.com/kowainik/extensions/issues/69
    previous <- getCurrentDirectory
    setCurrentDirectory (takeDirectory cabalPath)
    anyExts <- handleCabalException $ getPackageExtentions (takeFileName cabalPath)
    cabalExts' <- handleCabalException $ parseCabalFileExtensions (takeFileName cabalPath)
    let exts2 = Set.fromList $ concatMap parsedExtensionsAll cabalExts'

    setCurrentDirectory previous
    case mergeExtensions . concatMap (Set.toList . extensionsAll) <$> sequence (Map.elems anyExts) of
        Left err   -> do
            hPutStrLn stderr $ "Failed to parse files in " ++ dir ++ ":"
            hPrint stderr err
            return Nothing
        Right exts1 ->
            return (Just (exts1, exts2))
  let total = length sets
  let failures = length [ () | Nothing <- sets ]
  let parsed = total - failures
  let tally =
        sortOn snd $ Map.toList $ Map.unionsWith mappend $
        [ Map.fromSet (const (Sum 1,mempty)) s | Just (s,_) <- sets ] ++
        [ Map.fromSet (const (mempty, Sum 1)) s | Just (_, s) <- sets ]

  forM_ tally $ \(e, (Sum n1, Sum n2)) ->
    printf "%4d %4d %s\n" (n1::Int) (n2::Int) (showOnOffExtension e)
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

