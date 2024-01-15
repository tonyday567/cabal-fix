{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Various <https://hackage.haskell.org/package/flatparse flatparse> helpers and combinators.
module CabalFix.FlatParse
  ( -- * Parsing
    ParserWarning (..),
    runParserMaybe,
    runParserEither,
    runParserWarn,
    runParser_,
    runParser__,

    -- * Flatparse re-exports
    runParser,
    Parser,
    Result (..),

    -- * Parsers
    depP,
    versionP,
    versionInts,
    untilP,
    nota,
    ws_,
    ws,
  )
where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.Char hiding (isDigit)
import Data.These
import FlatParse.Basic hiding (cut, take)
import GHC.Exts
import GHC.Generics (Generic)
import Prelude hiding (replicate)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XOverloadedStrings
-- >>> import CabalFix.FlatParse
-- >>> import FlatParse.Basic

-- | Run a Parser, throwing away leftovers. Nothing on 'Fail' or 'Err'.
runParserMaybe :: Parser e a -> ByteString -> Maybe a
runParserMaybe p b = case runParser p b of
  OK r _ -> Just r
  Fail -> Nothing
  Err _ -> Nothing

-- | Run a Parser, throwing away leftovers. Returns Left on 'Fail' or 'Err'.
runParserEither :: (IsString e) => Parser e a -> ByteString -> Either e a
runParserEither p bs = case runParser p bs of
  Err e -> Left e
  OK a _ -> Right a
  Fail -> Left "uncaught parse error"

-- | Warnings covering leftovers, 'Err's and 'Fail'
data ParserWarning = ParserLeftover ByteString | ParserError ByteString | ParserUncaught deriving (Eq, Show, Ord, Generic, NFData)

-- | Run parser, returning leftovers and errors as 'ParserWarning's.
runParserWarn :: Parser ByteString a -> ByteString -> These ParserWarning a
runParserWarn p bs = case runParser p bs of
  Err e -> This (ParserError e)
  OK a "" -> That a
  OK a x -> These (ParserLeftover $ C.take 200 x) a
  Fail -> This ParserUncaught

-- | Run parser, discards leftovers & throws an error on failure.
--
-- >>> runParser_ ws " "
-- ' '
--
-- >>> runParser_ ws "x"
-- *** Exception: uncaught parse error
-- ...
runParser_ :: Parser String a -> ByteString -> a
runParser_ p bs = case runParser p bs of
  Err e -> error e
  OK a _ -> a
  Fail -> error "uncaught parse error"

-- | Run parser, errors on leftovers & failure.
--
-- >>> runParser_ ws " "
-- ' '
--
-- >>> runParser_ ws "x"
-- *** Exception: uncaught parse error
-- ...
runParser__ :: Parser String a -> ByteString -> a
runParser__ p bs = case runParser p bs of
  Err e -> error e
  OK a "" -> a
  OK _ x -> error $ "leftovers: " <> C.unpack (C.take 20 x)
  Fail -> error "uncaught parse error"

-- | Consume whitespace.
--
-- >>> runParser ws_ " \nx"
-- OK () "x"
--
-- >>> runParser ws_ "x"
-- OK () "x"
ws_ :: Parser e ()
ws_ =
  $( switch
       [|
         case _ of
           " " -> ws_
           "\n" -> ws_
           "\t" -> ws_
           "\r" -> ws_
           "\f" -> ws_
           _ -> pure ()
         |]
   )
{-# INLINE ws_ #-}

-- | \\n \\t \\f \\r and space
isWhitespace :: Char -> Bool
isWhitespace ' ' = True -- \x20 space
isWhitespace '\x0a' = True -- \n linefeed
isWhitespace '\x09' = True -- \t tab
isWhitespace '\x0c' = True -- \f formfeed
isWhitespace '\x0d' = True -- \r carriage return
isWhitespace _ = False
{-# INLINE isWhitespace #-}

-- | single whitespace
--
-- >>> runParser ws " \nx"
-- OK ' ' "\nx"
ws :: Parser e Char
ws = satisfy isWhitespace

-- | Parse whilst not a specific character
--
-- >>> runParser (nota 'x') "abcxyz"
-- OK "abc" "xyz"
nota :: Char -> Parser e ByteString
nota c = withSpan (skipMany (satisfy (/= c))) (\() s -> unsafeSpanToByteString s)
{-# INLINE nota #-}

-- | parse whilst not a specific character, then consume the character.
--
-- >>> runParser (untilP 'x') "abcxyz"
-- OK "abc" "yz"
untilP :: Char -> Parser e ByteString
untilP c = nota c <* satisfy (== c)

prefixComma :: Parser e ()
prefixComma = $(char ',') >> ws_

postfixComma :: Parser e ()
postfixComma = ws_ >> $(char ',')

initialPackageChar :: Parser e Char
initialPackageChar =
  satisfyAscii
    ( `C.elem`
        ( C.pack $
            ['a' .. 'z']
              <> ['A' .. 'Z']
              <> ['0' .. '9']
        )
    )

packageChar :: Parser e Char
packageChar =
  satisfyAscii
    ( `C.elem`
        ( C.pack $
            ['a' .. 'z']
              <> ['A' .. 'Z']
              <> ['-']
              <> ['0' .. '9']
        )
    )

validName :: Parser e String
validName = (:) <$> initialPackageChar <*> many packageChar

-- | Parse a dependency line into a name, range tuple. Consumes any commas it finds.
depP :: Parser e (ByteString, ByteString)
depP =
  (,)
    <$> ( optional prefixComma
            *> ws_
            *> byteStringOf validName
            <* ws_
        )
    <*> nota ','
    <* optional postfixComma

-- | Parse a version bytestring ti an int list.
versionP :: Parser e [Int]
versionP = (:) <$> int <*> many ($(char '.') >> int)

-- | A single digit
digit :: Parser e Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

-- | An (unsigned) 'Int' parser
int :: Parser e Int
int = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

-- | partial running of versionP
--
-- >>> versionInts "0.6.10.0"
-- [0,6,10,0]
versionInts :: ByteString -> [Int]
versionInts x = runParser__ versionP x
