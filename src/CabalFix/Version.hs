{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- |

module CabalFix.Version where

import FlatParse.Basic qualified as FP
import GHC.Generics
import Data.ByteString (ByteString)
import NumHask.Space
import Data.Bool

newtype Version = Version [Int] deriving (Eq, Ord, Show, Generic)

prettyVersion :: Version -> String
prettyVersion (Version xs) = List.intercalate "." (show <$> xs)

-- | A single digit
--
-- runParserMaybe digit "5"
-- Just 5
digit :: FP.Parser e Int
digit = (\c -> ord c - ord '0') <$> FP.satisfyAscii isDigit

-- | (unsigned) Int parser
--
-- >>> runParserMaybe int "567"
-- Just 567
int :: FP.Parser e Int
int = do
  (place, n) <- FP.chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> FP.empty
    _ -> pure n

versionP :: FP.Parser e Version
versionP = Version <$> intercalated int ($(FP.char '.'))

version :: ByteString -> Version
version = runP versionP

data EndPoint = Closed | Opened deriving (Eq, Ord, Show)

data RangeEP a = RangeEP { rangeEP :: Range a, lowerEP :: EndPoint, upperEP :: EndPoint} deriving (Eq, Generic, Show)

plusEpsilon :: Version -> Version
plusEpsilon (Version xs) = Version (xs <> [0])

minusEpsilon :: Version -> Version
minusEpsilon (Version []) = Version [0]
minusEpsilon (Version xs) = plusEpsilon $ Version $ reverse (go (reverse xs))
  where
    go [] = []
    go (x:xs) = bool (go xs) (x-1:xs) (x==0)

instance Space (RangeEP Version) where
  type Element (RangeEP Version) = Version
  lower r = bool (lower (rangeEP r)) (plusEpsilon $ lower (rangeEP r)) (lowerEP r == Closed)
  upper r = bool (upper (rangeEP r)) (minusEpsilon $ upper (rangeEP r)) (upperEP r == Closed)
  (>.<) l u = RangeEP (l >.< u) Opened Opened
  -- (|.|) a s = (a >= lower s) && (upper s >= a)
