{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Archive investigation

module CabalFix.Archive where

import qualified Codec.Archive.Tar as Tar
import System.Directory
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString (ByteString)
import FlatParse.Basic qualified as FP
import GHC.Generics
import Data.List qualified as List
import Data.Bool
import Data.Map.Strict qualified as Map
import Distribution.Fields
import Data.Bifunctor
import Data.Either
import Data.Maybe
import CabalFix ( sec )
import Distribution.Version
import Distribution.Parsec
import CabalFix.FlatParse

cabalIndex :: IO FilePath
cabalIndex = do
  h <- getHomeDirectory
  pure $ h <> "/.cabal/packages/hackage.haskell.org/01-index.tar"

entryList :: Tar.Entries Tar.FormatError -> [Tar.Entry]
entryList es = Tar.foldEntries (:) [] (error . show) es

cabalEntries :: IO [Tar.Entry]
cabalEntries = entryList . Tar.read <$> (BSL.readFile =<< cabalIndex)

isNormalFile :: Tar.EntryContent -> Bool
isNormalFile (Tar.NormalFile _ _) = True
isNormalFile _ = False

data FileName = FileName { nameFN :: ByteString, versionFN :: ByteString, filenameFN :: ByteString } deriving (Generic, Eq, Ord, Show)

filename :: ByteString -> FileName
filename = runParser_ filenameP

filenameP :: FP.Parser e FileName
filenameP = FileName <$> untilP '/' <*> untilP '/' <*> FP.takeRest

-- | cabal files
cabals :: IO [ (FileName, ByteString) ]
cabals = do
  es <- cabalEntries
  pure $ first (runParser_ filenameP . FP.strToUtf8) <$> filter ((/= "package.json") . filenameFN . runParser_ filenameP . FP.strToUtf8 . fst) (filter (not . List.isSuffixOf "preferred-versions" . fst) $ [(fp,BSL.toStrict bs) | (fp, Tar.NormalFile bs _) <- (\e -> (Tar.entryPath e, Tar.entryContent e)) <$> es])

latestCabals :: IO (Map.Map ByteString (Version, ByteString))
latestCabals = do
  cs <- CabalFix.Archive.cabals
  pure $ Map.fromListWith (\v v' -> bool v' v (fst v > fst v')) $ (\(fn,bs) -> (nameFN fn, (getVersion fn, bs))) <$> cs
  where
    getVersion = fromMaybe undefined . simpleParsecBS . versionFN

latestValidFields :: IO (Map.Map ByteString (Version, [Field Position]))
latestValidFields = do
  fmap (second (fromRight undefined)) . Map.filter (isRight . snd) . fmap (second readFields) <$> latestCabals

-- | valid cabal files with all fields parsing ok, and at least one library section.
validLatestLibs :: IO (Map.Map ByteString (Version, [Field Position]))
validLatestLibs = do
  Map.filter (not . null . mapMaybe (sec "library") . snd) <$> latestValidFields
