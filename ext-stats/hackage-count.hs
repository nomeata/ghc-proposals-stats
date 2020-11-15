{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
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
import Data.Maybe
import GHC.LanguageExtensions (Extension)
import GHC.Generics
import qualified Data.ByteString.Lazy as ByteString
import Data.Map hiding (take, filter)
import qualified Data.Map as Map

import Extensions
import Data.Aeson

main = do
  args <- getArgs
  sets <- forM args $ \dir -> do
    cabalPath <- findCabalFile dir
    case cabalPath of
      Nothing -> do
            hPutStrLn stderr $ "No cabal file found in " ++ dir
            return Nothing
      Just cabalPath -> do
        -- change cwd due to https://github.com/kowainik/extensions/issues/69
        previous <- getCurrentDirectory
        setCurrentDirectory (takeDirectory cabalPath)
        anyExts <- handleCabalException $ getPackageExtentions (takeFileName cabalPath)
        cabalExts' <- handleCabalException $ parseCabalFileExtensions (takeFileName cabalPath)
        let exts2 = Set.fromList $ concatMap parsedExtensionsAll cabalExts'

        setCurrentDirectory previous
        case mergeExtensions . concatMap (Set.toList . extensionsAll) <$> sequence (Map.elems anyExts) of
            Left (Extensions.ModuleParseError file err) -> do
                hPutStrLn stderr $ "Failed to parse " ++ takeDirectory cabalPath </> file ++ ":"
                hPrint stderr err
                return Nothing
            Left err   -> do
                hPutStrLn stderr $ "Failed to parse files in " ++ dir ++ ":"
                hPrint stderr err
                return Nothing
            Right exts1 ->
                return (Just (exts1, exts2))
  let total = length sets
  let failures = length [ () | Nothing <- sets ]
  let parsed = total - failures
  let any_in_cabal = length [ () | Just (_, s2) <- sets, not (Set.null s2) ]
  let tally =
        sortOn snd $ Map.toList $ Map.unionsWith mappend $
        [ Map.fromSet (const (Sum 1,mempty,mempty)) s | Just (s,_) <- sets ] ++
        [ Map.fromSet (const (mempty, Sum 1,mempty)) s | Just (_, s) <- sets ] ++
        [ Map.fromSet (const (mempty, mempty, Sum 1)) (s1 `Set.difference` s2) | Just (s1, s2) <- sets, not (Set.null s2) ]

  forM_ tally $ \(e, (Sum n1, Sum n2, Sum n3)) ->
    printf "%s %s %s %s\n"
      (n1 `outOf` parsed) (n2 `outOf` any_in_cabal) (n3 `outOf` any_in_cabal)
      (showOnOffExtension e)
  printf "--------------------\n"
  printf "%4d packages total\n" total
  printf "%4d packages parsed\n" parsed
  printf "%4d packages with extensions in cabal file\n" any_in_cabal
  ByteString.writeFile "data.json" $ encode $ mconcat $ mkE <$> catMaybes sets

data Tally = Tally { inFiles :: OO, inCabal :: OO }

mkE :: (Set.Set OnOffExtension, Set.Set OnOffExtension) -> Tally
mkE (a, b) = Tally (foldMap runOff a) (foldMap runOff b)

data OO = OO (Map String Int) (Map String Int)
instance ToJSON OO where
  toJSON (OO a b) = object [ "turned-off" .= toJSON a, "turned-on" .= toJSON b ]

instance Semigroup OO where
  OO a0 b0 <> OO a1 b1 = OO (Map.unionWith (+) a0 a1) (Map.unionWith (+) b0 b1)
instance Monoid OO where
  mempty = OO mempty mempty

runOff :: OnOffExtension -> OO
runOff = \case
  Off e -> OO (Map.singleton (show e) 1) mempty
  On e -> OO mempty (Map.singleton (show e) 1)

instance Semigroup Tally where
  Tally a0 b0 <> Tally a1 b1 = Tally (a0 <> a1) (b0 <> b1)
instance Monoid Tally where
  mempty = Tally mempty mempty
deriving stock instance Generic Tally
instance ToJSON Tally where
  toJSON (Tally a b) = object [ "enabled-in-cabal" .= toJSON a, "enabled-in-file" .= toJSON b ]

outOf :: Int -> Int -> String
outOf x 0 = "---%"
outOf x y = printf "%2.0f%%" (fromIntegral x / fromIntegral y * 100 :: Double)

findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile dirPath = do
    dirContent <- listDirectory dirPath
    case filter (\p -> take 1 p /= "." && takeExtension p == ".cabal") dirContent of
        [] -> return Nothing
        -- lets be more liberal than the extensions tool, and just take the first
        (cabalFile:_) -> return $ Just $ dirPath </> cabalFile

exitAfter :: IO a -> IO b
exitAfter action = action >> exitFailure

handleCabalException :: IO a -> IO a
handleCabalException action = action `catch` \exc -> exitAfter $
    hPutStrLn stderr $ "Error processing .cabal file: " <> show (exc :: Extensions.CabalException)

