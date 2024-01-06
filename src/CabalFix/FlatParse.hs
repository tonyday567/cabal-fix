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

    -- * Flatparse re-exports
    runParser,
    Parser,
    Result (..),

    -- * Parsers
    depP,
    untilP,
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
-- >>> import MarkupParse.FlatParse
-- >>> import FlatParse.Basic


-- | Run a Parser, throwing away leftovers. Nothing on 'Fail' or 'Err'.
--
-- >>> runParserMaybe ws "x"
-- Nothing
--
-- >>> runParserMaybe ws " x"
-- Just ' '
runParserMaybe :: Parser e a -> ByteString -> Maybe a
runParserMaybe p b = case runParser p b of
  OK r _ -> Just r
  Fail -> Nothing
  Err _ -> Nothing

-- | Run a Parser, throwing away leftovers. Returns Left on 'Fail' or 'Err'.
--
-- >>> runParserEither ws " x"
-- Right ' '
runParserEither :: (IsString e) => Parser e a -> ByteString -> Either e a
runParserEither p bs = case runParser p bs of
  Err e -> Left e
  OK a _ -> Right a
  Fail -> Left "uncaught parse error"

-- | Warnings covering leftovers, 'Err's and 'Fail'
--
-- >>> runParserWarn ws " x"
-- These (ParserLeftover "x") ' '
--
-- >>> runParserWarn ws "x"
-- This ParserUncaught
--
-- >>> runParserWarn (ws `cut` "no whitespace") "x"
-- This (ParserError "no whitespace")
data ParserWarning = ParserLeftover ByteString | ParserError ByteString | ParserUncaught deriving (Eq, Show, Ord, Generic, NFData)

-- | Run parser, returning leftovers and errors as 'ParserWarning's.
--
-- >>> runParserWarn ws " "
-- That ' '
--
-- >>> runParserWarn ws "x"
-- This ParserUncaught
--
-- >>> runParserWarn ws " x"
-- These (ParserLeftover "x") ' '
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


-- | Parse whilst not a specific character
--
-- >>> runParser (nota 'x') "abcxyz"
-- OK "abc" "xyz"
nota :: Char -> Parser e ByteString
nota c = withSpan (skipMany (satisfy (/= c))) (\() s -> unsafeSpanToByteString s)
{-# INLINE nota #-}

untilP :: Char -> Parser e ByteString
untilP c = nota c <* satisfy (==c)

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
