{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | basic measurement and callibration
module Main where

import CabalFix
import Data.TreeDiff
import MarkupParse.Patch
import Options.Applicative
import System.Directory
import System.FilePath
import Prelude
import Data.Bool
import Text.Pretty.Simple
import Data.Text.Lazy.IO qualified as Text

data FixOptions = FixOptions
  { fixInplace :: Bool,
    configFile :: FilePath,
    genConfigFile :: Bool
  }
  deriving (Eq, Show)

defaultConfigFilePath :: FilePath
defaultConfigFilePath = "cabal-fix.config"

parseFixOptions :: Parser FixOptions
parseFixOptions =
  FixOptions <$>
    switch (long "inplace" <> short 'i' <> help "fix the cabal file inplace") <*>
    option str (Options.Applicative.value defaultConfigFilePath <> long "configFile" <> short 'f' <> help "config file") <*>
    switch (long "gen" <> short 'g' <> help "generate config file")

infoFixOptions :: ParserInfo FixOptions
infoFixOptions =
  info
    (parseFixOptions <**> helper)
    (fullDesc <> progDesc "cabal fixer" <> header "fixes your cabal file")

main :: IO ()
main = do
  o <- execParser infoFixOptions
  bool (runRender o) (Text.writeFile (o.configFile) (pShowNoColor defaultRenderConfig)) (genConfigFile o)

  where
    runRender o = do
      cfg <- read <$> readFile (o.configFile)
      let inplace = fixInplace o
      d <- getCurrentDirectory
      let fp = takeBaseName d <> ".cabal"
      exists <- doesFileExist fp
      case exists of
        False -> putStrLn "cabal file not found"
        True -> do
          case inplace of
            True -> rerenderFile fp cfg
            False -> do
              bs <- getCabal fp
              let bs' = rerenderCabal defaultRenderConfig bs
              print $ ansiWlEditExpr <$> patch bs bs'
