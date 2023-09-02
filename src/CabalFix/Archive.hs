{-# LANGUAGE TemplateHaskell #-}
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
import NumHask.Space
import Data.List qualified as List
import Data.Char
import Data.Bool
import Data.String
import Data.Map.Strict qualified as Map
import Distribution.Fields
import Distribution.Parsec.Position (Position)
import Data.Bifunctor
import Data.Either
import Data.Maybe
import CabalFix

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
filename = runP filenameP

filenameP :: FP.Parser e FileName
filenameP = FileName <$> untilP '/' <*> untilP '/' <*> FP.takeRest

runP :: FP.Parser e a -> ByteString -> a
runP p bs = case FP.runParser p bs of
  FP.OK res _ -> res
  _ -> error "parse error"

-- | run a Parser, throwing away leftovers. Returns Left on 'Fail' or 'Err'.
--
-- >>> runParserEither ws " x"
-- Right ' '
runParserEither :: (IsString e) => FP.Parser e a -> ByteString -> Either e a
runParserEither p bs = case FP.runParser p bs of
  FP.Err e -> Left e
  FP.OK a _ -> Right a
  FP.Fail -> Left "uncaught parse error"

runParserEither' :: FP.Parser ByteString a -> ByteString -> Either ByteString a
runParserEither' p bs = case FP.runParser p bs of
  FP.Err e -> Left e
  FP.OK a "" -> Right a
  FP.OK _ l -> Left ("leftovers: " <> l)
  FP.Fail -> Left "uncaught parse error"


-- | cabal files
cabals :: IO [ (FileName, ByteString) ]
cabals = do
  es <- cabalEntries
  pure $ first (runP filenameP . FP.strToUtf8) <$> filter ((/= "package.json") . filenameFN . runP filenameP . FP.strToUtf8 . fst) (filter (not . List.isSuffixOf "preferred-versions" . fst) $ [(fp,BSL.toStrict bs) | (fp, Tar.NormalFile bs _) <- (\e -> (Tar.entryPath e, Tar.entryContent e)) <$> es])

latestCabals :: IO (Map.Map ByteString (Version, ByteString))
latestCabals = do
  cs <- CabalFix.Archive.cabals
  pure $ Map.fromListWith (\v v' -> bool v' v (fst v > fst v')) $ (\(fn,bs) -> (nameFN fn, (getVersion fn, bs))) <$> cs
  where
    getVersion = runP versionP . versionFN

latestValidFields :: IO (Map.Map ByteString (Version, [Field Position]))
latestValidFields = do
  fmap (second (fromRight undefined)) . Map.filter (isRight . snd) . fmap (second readFields) <$> latestCabals

-- | valid cabal files with all fields parsing ok, and at least one library section.
validLatestLibs :: IO (Map.Map ByteString (Version, [Field Position]))
validLatestLibs = do
  Map.filter (not . null . mapMaybe (sec "library") . snd) <$> latestValidFields
