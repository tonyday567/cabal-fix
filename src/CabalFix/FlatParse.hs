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
    isWhitespace,
    ws_,
    ws,
    wss,
    nota,
    untilP,
    isa,
    sq,
    dq,
    wrappedDq,
    wrappedSq,
    wrappedQ,
    wrappedQNoGuard,
    eq,
    sep,
    bracketed,
    bracketedSB,
    wrapped,
    digit,
    int,
    double,
    signed,
    byteStringOf',
    comma,
    prefixComma,
    postfixComma,
    comma_,

    -- * specific version parsing
    lt,
    lte,
    gt,
    gte,
    caret,
    amps,
    intercalated,

    validName,
    version,
    depP,
  )
where

import Control.DeepSeq
import Data.Bool
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

-- | multiple whitespace
--
-- >>> runParser wss " \nx"
-- OK " \n" "x"
--
-- >>> runParser wss "x"
-- Fail
wss :: Parser e ByteString
wss = byteStringOf $ some ws

-- | Single quote
--
-- >>> runParserMaybe sq "'"
-- Just ()
sq :: ParserT st e ()
sq = $(char '\'')

-- | Double quote
--
-- >>> runParserMaybe dq "\""
-- Just ()
dq :: ParserT st e ()
dq = $(char '"')

-- | Parse whilst not a specific character
--
-- >>> runParser (nota 'x') "abcxyz"
-- OK "abc" "xyz"
nota :: Char -> Parser e ByteString
nota c = withSpan (skipMany (satisfy (/= c))) (\() s -> unsafeSpanToByteString s)
{-# INLINE nota #-}

untilP :: Char -> Parser e ByteString
untilP c = nota c <* satisfy (==c)

-- | Parse whilst satisfying a predicate.
--
-- >>> runParser (isa (=='x')) "xxxabc"
-- OK "xxx" "abc"
isa :: (Char -> Bool) -> Parser e ByteString
isa p = withSpan (skipMany (satisfy p)) (\() s -> unsafeSpanToByteString s)
{-# INLINE isa #-}

-- | 'byteStringOf' but using withSpan internally. Doesn't seems faster...
byteStringOf' :: Parser e a -> Parser e ByteString
byteStringOf' p = withSpan p (\_ s -> unsafeSpanToByteString s)
{-# INLINE byteStringOf' #-}

-- | A single-quoted string.
wrappedSq :: Parser b ByteString
wrappedSq = $(char '\'') *> nota '\'' <* $(char '\'')
{-# INLINE wrappedSq #-}

-- | A double-quoted string.
wrappedDq :: Parser b ByteString
wrappedDq = $(char '"') *> nota '"' <* $(char '"')
{-# INLINE wrappedDq #-}

-- | A single-quoted or double-quoted string.
--
-- >>> runParserMaybe wrappedQ "\"quoted\""
-- Just "quoted"
--
-- >>> runParserMaybe wrappedQ "'quoted'"
-- Just "quoted"
wrappedQ :: Parser e ByteString
wrappedQ =
  wrappedDq
    <|> wrappedSq
{-# INLINE wrappedQ #-}

-- | A single-quoted or double-quoted wrapped parser.
--
-- >>> runParser (wrappedQNoGuard (many $ satisfy (/= '"'))) "\"name\""
-- OK "name" ""
--
-- Will consume quotes if the underlying parser does.
--
-- >>> runParser (wrappedQNoGuard (many anyChar)) "\"name\""
-- Fail
wrappedQNoGuard :: Parser e a -> Parser e a
wrappedQNoGuard p = wrapped dq p <|> wrapped sq p

-- | xml production [25]
--
-- >>> runParserMaybe eq " = "
-- Just ()
--
-- >>> runParserMaybe eq "="
-- Just ()
eq :: Parser e ()
eq = ws_ *> $(char '=') <* ws_
{-# INLINE eq #-}

-- | Some with a separator.
--
-- >>> runParser (sep ws (many (satisfy (/= ' ')))) "a b c"
-- OK ["a","b","c"] ""
sep :: Parser e s -> Parser e a -> Parser e [a]
sep s p = (:) <$> p <*> many (s *> p)

-- | Parser bracketed by two other parsers.
--
-- >>> runParser (bracketed ($(char '[')) ($(char ']')) (many (satisfy (/= ']')))) "[bracketed]"
-- OK "bracketed" ""
bracketed :: Parser e b -> Parser e b -> Parser e a -> Parser e a
bracketed o c p = o *> p <* c
{-# INLINE bracketed #-}

-- | Parser bracketed by square brackets.
--
-- >>> runParser bracketedSB "[bracketed]"
-- OK "bracketed" ""
bracketedSB :: Parser e [Char]
bracketedSB = bracketed $(char '[') $(char ']') (many (satisfy (/= ']')))

-- | Parser wrapped by another parser.
--
-- >>> runParser (wrapped ($(char '"')) (many (satisfy (/= '"')))) "\"wrapped\""
-- OK "wrapped" ""
wrapped :: Parser e () -> Parser e a -> Parser e a
wrapped x p = bracketed x x p
{-# INLINE wrapped #-}

-- | A single digit
--
-- >>> runParserMaybe digit "5"
-- Just 5
digit :: Parser e Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

-- | An (unsigned) 'Int' parser
--
-- >>> runParserMaybe int "567"
-- Just 567
int :: Parser e Int
int = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

digits :: Parser e (Int, Int)
digits = chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))

-- | A 'Double' parser.
--
-- >>> runParser double "1.234x"
-- OK 1.234 "x"
--
-- >>> runParser double "."
-- Fail
--
-- >>> runParser double "123"
-- OK 123.0 ""
--
-- >>> runParser double ".123"
-- OK 0.123 ""
--
-- >>> runParser double "123."
-- OK 123.0 ""
double :: Parser e Double
double = do
  (placel, nl) <- digits
  withOption
    ($(char '.') *> digits)
    ( \(placer, nr) ->
        case (placel, placer) of
          (1, 1) -> empty
          _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer
    )
    ( case placel of
        1 -> empty
        _ -> pure $ fromIntegral nl
    )

minus :: Parser e ()
minus = $(char '-')

-- | Parser for a signed prefix to a number.
--
-- >>> runParser (signed double) "-1.234x"
-- OK (-1.234) "x"
signed :: (Num b) => Parser e b -> Parser e b
signed p = do
  m <- optional minus
  case m of
    Nothing -> p
    Just () -> negate <$> p

-- | Comma parser
--
-- >>> runParserMaybe comma ","
-- Just ()
comma :: Parser e ()
comma = $(char ',')

prefixComma :: Parser e ()
prefixComma = $(char ',') >> ws_

postfixComma :: Parser e ()
postfixComma = ws_ >> $(char ',')

comma_ :: Parser e ()
comma_ = ws_ >> $(char ',') >> ws_

-- * cabal-fix specific
caret :: Parser e ()
caret = $(string "^>=")

gte :: Parser e ()
gte = $(string ">=")

gt :: Parser e ()
gt = $(string ">")

lt :: Parser e ()
lt = $(string "<")

lte :: Parser e ()
lte = $(string "<=")

amps :: Parser e ()
amps = $(string "&&")

intercalated :: Parser e item -> Parser e sep -> Parser e [item]
intercalated item sep' =
  (:) <$> item <*> chainr (:) (sep' *> item) (pure [])

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

versionChar :: Parser e Char
versionChar =
  satisfyAscii
    ( `C.elem`
        ( C.pack $
            ['.']
              <> ['0' .. '9']
        )
    )

version :: Parser e ByteString
version = byteStringOf (some versionChar)

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
