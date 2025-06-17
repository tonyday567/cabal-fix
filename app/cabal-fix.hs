{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | cabal-fix app
module Main where

import CabalFix
  ( Config,
    defaultConfig,
    fixCabalFields,
    fixCabalFile,
    parseCabalFields,
    printCabalFields,
  )
import Control.Monad
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.Text.Lazy.IO qualified as Text
import GHC.Generics
import Options.Applicative
import System.Directory
import System.FilePath
import Text.Pretty.Simple
import Prelude
import Data.Algorithm.DiffOutput
import Data.Algorithm.Diff
import Data.Function ((&))
import Control.Category ((>>>))
import Data.Bifunctor


data CommandType = FixInplace | FixCheck | GenerateConfig deriving (Generic, Eq, Show)

data Options = Options
  { commandType :: CommandType,
    projectDir :: FilePath,
    configFile :: FilePath
  }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options FixCheck "." "cabal-fix.config"

parseCommand :: Parser CommandType
parseCommand =
  subparser $
    command "inplace" (info (pure FixInplace) (progDesc "fix cabal file inplace"))
      <> command "check" (info (pure FixCheck) (progDesc "check cabal file"))
      <> command "genConfig" (info (pure GenerateConfig) (progDesc "generate config file"))

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseCommand
    <*> option str (Options.Applicative.value (projectDir defaultOptions) <> long "directory" <> short 'd' <> help "project directory")
    <*> option str (Options.Applicative.value (configFile defaultOptions) <> long "config" <> short 'c' <> help "config file")

infoOptions :: ParserInfo Options
infoOptions =
  info
    (parseOptions <**> helper)
    (fullDesc <> progDesc "cabal fixer" <> header "fixes your cabal file")

main :: IO ()
main = do
  o <- execParser infoOptions
  runCabalFix o

runCabalFix :: Options -> IO ()
runCabalFix o = do
  case commandType o of
    GenerateConfig ->
      Text.writeFile (configFile o) (pShowNoColor defaultConfig)
    FixInplace -> do
      cfg <- getConfig o
      fp <- getCabalFile o
      b <- fixCabalFile fp cfg
      unless b (putStrLn "parsing failure")
    FixCheck -> do
      cfg <- getConfig o
      fp <- getCabalFile o
      bs <- BS.readFile fp
      let bs' = printCabalFields cfg . fixCabalFields cfg <$> parseCabalFields cfg bs
      print $ ediff bs <$> bs'

ediff expected actual = getDiff (C.lines expected) (C.lines actual) & fmap (bimap (C.unpack >>> pure) (C.unpack >>> pure)) & diffToLineRanges & prettyDiffs


getConfig :: Options -> IO Config
getConfig o = do
  configExists <- doesFileExist (configFile o)
  bool (pure defaultConfig) (read <$> readFile (configFile o)) configExists

getCabalFile :: Options -> IO FilePath
getCabalFile o = do
  fs <- getDirectoryContents (projectDir o)
  case filter ((== ".cabal") . takeExtension) fs of
    [] -> error "No .cabal file found"
    [c] -> pure (projectDir o </> c)
    _ -> error "multiple .cabal files found"
