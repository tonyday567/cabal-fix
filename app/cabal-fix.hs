{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Options.Applicative
import CabalFix
import Prelude
import System.FilePath
import System.Directory
import Data.TreeDiff
import MarkupParse.Patch

data FixOptions = FixOptions
  { fixInplace :: Bool
  }
  deriving (Eq, Show)

parseFixOptions :: Parser FixOptions
parseFixOptions =
  FixOptions <$> switch (long "inplace" <> short 'i' <> help "fix the cabal file inplace")

infoFixOptions :: ParserInfo FixOptions
infoFixOptions =
  info
    (parseFixOptions <**> helper)
    (fullDesc <> progDesc "cabal fixer" <> header "fixes your cabal file")

main :: IO ()
main = do
  let cfg = defaultRenderConfig
  o <- execParser infoFixOptions
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
