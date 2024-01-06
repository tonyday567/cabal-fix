{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import CabalFix
    ( defaultConfig, getCabalFile, cabalFix, cabalFixFile )
import Data.TreeDiff
import MarkupParse.Patch
import Options.Applicative
import System.Directory
import System.FilePath
import Prelude
import Data.Bool
import Text.Pretty.Simple
import Data.Text.Lazy.IO qualified as Text
import GHC.Generics

data FixType = FixInplace | FixCheck deriving (Generic, Eq, Show)

data Options = Options
  { fixType :: FixType,
    configFile :: FilePath,
    genConfigFile :: Bool
  }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options FixCheck "cabal-fix.config" False

parseOptions :: Parser Options
parseOptions =
  Options <$>
    (bool FixCheck FixInplace <$> switch (long "inplace" <> short 'i' <> help "fix the cabal file inplace")) <*>
    option str (Options.Applicative.value (configFile defaultOptions) <> long "config" <> short 'c' <> help "config file") <*>
    switch (long "gen" <> short 'g' <> help "generate config file")

infoOptions :: ParserInfo Options
infoOptions =
  info
    (parseOptions <**> helper)
    (fullDesc <> progDesc "cabal fixer" <> header "fixes your cabal file")

main :: IO ()
main = do
  o <- execParser infoOptions
  bool (runCabalFix o) (Text.writeFile (configFile o) (pShowNoColor defaultConfig)) (genConfigFile o)

runCabalFix :: Options -> IO ()
runCabalFix o = do
  configExists <- doesFileExist (configFile o)
  cfg <- bool (pure defaultConfig) (read <$> readFile (configFile o)) configExists
  d <- getCurrentDirectory
  let fp = takeBaseName d <> ".cabal"
  exists <- doesFileExist fp
  case exists of
    False -> putStrLn "cabal file not found"
    True -> do
      case fixType o of
        FixInplace -> cabalFixFile fp cfg
        FixCheck -> do
          bs <- getCabalFile fp
          let bs' = cabalFix cfg bs
          print $ ansiWlEditExpr <$> patch bs bs'
