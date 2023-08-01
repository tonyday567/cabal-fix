{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cabal.Fix
  (
    rerenderCabal,
    cabalsEmitter,
    allCabals',
    fieldE,
    allFields,
    toContents,
    toFields,
    licenseFile,
    cabals,
    isSection,
    name,
    fieldNames,
    author,
    value,
    fieldValue,
    fieldValues,
    fieldValues',
    sec,
    seci,
    secName,
    rawBuildDeps,
    count_,
    collect_,
    yearList,
    printFields,
    descriptionPreserveNewlines,
    showFields'',
    preserveNewlines,
    renderFields,
    renderField,
    ftField,
    hasPrefixComma,
    hasPrefixComma0,
    hasPostfixComma,
    anyField,
    whichFields,
    noCommas,
    prefixCommas,
    isBlankField,
    blankFields,
    removeFields,
    removeFieldss,
    AddPolicy (..),
    addField,
    addFieldLine,
    renderCabal,
    licenseF,
    licenseFileF,
    readCabalFile,
    writeCabalFile,
    depRangeP,
    depP,
    parseOK,
    printDep,
    printDepRange,
    preferredDeps,
    subPreferredDeps,
    subPreferredDeps',
    fieldOrdering,
    sortInnerFields,
    sortFields,
    sortFieldLines,
    attachComments,
    unComments,
    nullComments,
    fieldPathSize,
    comments'',
    makePositionTree,
    parseFieldsAndComments,
    connectIndexAction,
    addComment,
    allCabalProjects,
    projectCabal,
    allCabals,
  )
where

import Prelude
import Distribution.Fields
import Distribution.Fields.Field hiding (fieldUniverse)
import Distribution.Parsec.Position (Position)
import FlatParse.Basic (Parser)
import FlatParse.Basic qualified as FP
import System.Directory
import Data.String.Interpolate
import Box
import Data.Function
import Control.Monad
import Data.Bifunctor
import Data.List qualified as List
import GHC.Generics
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Map.Strict qualified as Map
import Data.Foldable
import Data.Maybe
import Data.Bool
import Text.PrettyPrint qualified as PP
import Distribution.Pretty
import Data.Functor.Identity
import Distribution.Utils.Generic
import Distribution.Parsec.Position
import qualified Data.ByteString  as BS
import FlatParse.Basic (Parser)
import FlatParse.Basic qualified as FP
import Control.Category ((>>>))

allCabalProjects :: FilePath -> IO [String]
allCabalProjects d = do
  cs <- getDirectoryContents d
  let cs' = filter (\x -> x /= "." && x /= "..") cs
  ps <- filterM (doesDirectoryExist . (\x -> path [d,x])) cs'
  ns <- filterM (doesFileExist . cabalPath d) ps
  pure ns

projectCabal :: FilePath -> String -> IO ByteString
projectCabal d p = BS.readFile (cabalPath d p)

allCabals :: FilePath -> IO [(String, ByteString)]
allCabals d = do
  ps <- allCabalProjects d
  mapM (\x -> (x,) <$> projectCabal d x) ps

-- cEmit :: FilePath -> CoEmitter IO (FilePath, String)
cabalsEmitter :: FilePath -> IO (CoEmitter IO (FilePath, String))
cabalsEmitter dir = do
  h <- getHomeDirectory
  ps <- cabals h dir
  pure $ witherE (fmap Just . (\x -> (x,) <$> readFile x) . cabalPath (path [h,dir])) <$> (qList ps)

allCabals' :: IO [(FilePath, String)]
allCabals' = do
  e <- cabalsEmitter "haskell"
  toListM <$|> e

fieldE :: Emitter IO (t, String) -> Emitter IO (t, [Field Position])
fieldE =
  witherE (\(fp,c) -> pure $ either (const Nothing) (Just . (fp,)) (readFields . C.pack $ c))

readCabalFile :: FilePath -> IO [Field Position]
readCabalFile fp = do
  bs <- toContents fp
  pure $ toFields bs

toContents :: FilePath -> IO ByteString
toContents fp = BS.readFile fp

toFields :: ByteString -> [Field Position]
toFields bs = either (error . show) id $ readFields bs

writeCabalFile :: FilePath -> [Field Position] -> IO ()
writeCabalFile fp fs = writeFile fp (renderCabal fs)

allFields :: IO [(FilePath, [Field Position])]
allFields = do
  e <- cabalsEmitter "haskell"
  toListM <$|> fieldE <$> e


cabals :: FilePath -> FilePath -> IO [String]
cabals h d = do
  cs <- getDirectoryContents (path [h,d])
  let cs' = filter (\x -> x /= "." && x /= "..") cs
  ps <- filterM (doesDirectoryExist . (\x -> path [h,d,x])) cs'
  ns <- filterM (doesFileExist . cabalPath (path[h,d])) ps
  pure ns

cabalPath :: FilePath -> FilePath -> FilePath
cabalPath d x = path [d,x,x <> ".cabal"]

path :: [FilePath] -> FilePath
path fps = List.intercalate "/" fps

-- | Unification of field and section names
name :: Field a -> ByteString
name (Field (Name _ n) _) = n
name (Section (Name _ n) _ _) = n

isSection :: Field a -> Bool
isSection (Field _ _) = False
isSection _ = True

fieldNames :: Field a -> [ByteString]
fieldNames (Field (Name _ n) _) = [n]
fieldNames (Section _ _ fss) = (mconcat $ fieldNames <$> fss)

author :: Field a -> [ByteString]
author (Field (Name _ "author") xs) = fieldLineBS <$> xs
author _ = []

fieldValues' :: Field a -> [ByteString]
fieldValues' (Field _ fs) = fieldLineBS <$> fs
fieldValues' _ = []

-- | extract a field's values, if any
fieldValue :: ByteString -> Field a -> [ByteString]
fieldValue f (Field (Name _ n) xs) = bool [] (fieldLineBS <$> xs) (f == n)
fieldValue _ _ = []

-- | extract a field's values, if any
value :: Field a -> [ByteString]
value (Field _ xs) = fieldLineBS <$> xs
value (Section _ _ fs) = mconcat (value <$> fs)

-- | extract a field values from a list, if any
fieldValues :: ByteString -> [Field a] -> [ByteString]
fieldValues v xs = mconcat $ fmap (fieldValue v) xs

-- | section deconstruction
sec :: FieldName -> Field ann -> Maybe ([SectionArg ann], [Field ann])
sec f (Section (Name _ n) sargs fs) = bool Nothing (Just (sargs, fs)) (f == n)
sec _ (Field _ _) = Nothing

seci :: Field ann -> Int -> Maybe (Field ann)
seci (Field _ _) _ = Nothing
seci (Section _ _ fss) i = Just $ fss List.!! i

-- | SectionArg name
secName :: SectionArg a -> (ByteString, ByteString)
secName (SecArgName _ n) = ("name", n)
secName (SecArgStr _ n) = ("str", n)
secName (SecArgOther _ n) = ("other", n)

-- | extract build-deps from a Field list, also looking in common stanzas
rawBuildDeps :: [Field a] -> [[ByteString]]
rawBuildDeps xs =
  bdeps <> bdepImports
  where
    libs = fmap snd . mapMaybe (sec "library") $ xs
    bdeps = fmap (fieldValues "build-depends") libs
    libImports = fmap (fieldValues "import") libs
    common = mapMaybe (sec "common") xs
    cbdMap =
      Map.fromList $
        fmap
          (bimap (fromJust . listToMaybe . fmap (snd . secName)) (fieldValues "build-depends"))
          common
    bdepImports =
      fmap
        ( mconcat
            . fmap (\x -> fromMaybe [] $ Map.lookup x cbdMap)
        )
        libImports

-- * streamly 'Fold's

count_ :: (Ord a) => [a] -> Map.Map a Int
count_ = foldl' (\x a -> Map.insertWith (+) a 1 x) Map.empty

collect_ :: (Ord k) => [(k,v)] -> Map.Map k [v]
collect_ = foldl' (\x (k,v) -> Map.insertWith (<>) k [v] x) Map.empty

-- | BSD3 clause from cabal init
licenseFile :: String -> String -> String
licenseFile a y = [i|
Copyright (c) #{y}, #{a}

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of #{a} nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|]

yearList :: [(String, Int)]
yearList = [("numhask",2016),("mealy",2013),("box",2017),("formatn",2016),("prettychart",2023),("code",2023),("poker-fold",2020),("numhask-space",2016),("iqfeed",2014),("box-socket",2017),("numhask-array",2016),("euler",2023),("tonyday567",2020),("foo",2023),("web-rep",2015),("dotparse",2022),("perf",2018),("anal",2023),("research-hackage",2022),("chart-svg",2017),("ephemeral",2020)]

printFields :: [Field ann] -> [PrettyField ann]
printFields = runIdentity . genericFromParsecFields
    (Identity .: prettyFieldLines)
    (Identity .: prettySectionArgs)
  where
    (.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
    (f .: g) x y = f (g x y)

convertDescription :: Field Position -> Field Position
convertDescription (Field n@(Name _ "description") fls) = Field n (convertToFreeText fls)
convertDescription f@(Field _ _) = f
convertDescription (Section n a fss) = Section n a (convertDescription <$> fss)

descriptionPreserveNewlines :: FieldName -> [FieldLine Position] -> PP.Doc
descriptionPreserveNewlines "description" fs = prettyFieldLines "description" (convertToFreeText fs)
descriptionPreserveNewlines n fs = prettyFieldLines n fs

preserveNewlines :: [FieldLine Position] -> PP.Doc
preserveNewlines [] = PP.empty
preserveNewlines ((FieldLine (Position r0 _) bs0):xs) = PP.vcat $ snd $ foldl' (\(r',xs') (FieldLine (Position r _) bs) -> (r, xs' <> (replicate (r-r'-1) (PP.text "\n")) <> [PP.text $ fromUTF8BS bs])) (r0,[PP.text $ fromUTF8BS bs0]) xs

prettyFieldLines :: FieldName -> [FieldLine ann] -> PP.Doc
prettyFieldLines _ fls = PP.vcat
    [ PP.text $ fromUTF8BS bs
    | FieldLine _ bs <- fls
    ]

prettySectionArgs :: FieldName -> [SectionArg ann] -> [PP.Doc]
prettySectionArgs _ = fmap $ \case
    SecArgName _ bs  -> showToken $ fromUTF8BS bs
    SecArgStr _ bs   -> showToken $ fromUTF8BS bs
    SecArgOther _ bs -> PP.text $ fromUTF8BS bs

-- | 'showFields' with user specified indentation.
showFields''
  :: (ann -> CommentPosition)
     -- ^ Convert an annotation to lined to preceed the field or section.
  -> (ann -> [String] -> [String])
     -- ^ Post-process non-annotation produced lines.
  -> Int
     -- ^ Indentation level.
  -> [PrettyField ann]
     -- ^ Fields/sections to show.
  -> String
showFields'' rann post n = unlines . renderFields (Opts rann indent post)
  where
    -- few hardcoded, "unrolled"  variants.
    indent | n == 4    = indent4
           | n == 2    = indent2
           | otherwise = (replicate (max n 1) ' ' ++)

    indent4 :: String -> String
    indent4 [] = []
    indent4 xs = ' ' : ' ' : ' ' : ' ' : xs

    indent2 :: String -> String
    indent2 [] = []
    indent2 xs = ' ' : ' ' : xs

data Opts ann = Opts
  { _optAnnotation :: ann -> CommentPosition
  , _optIndent :: String -> String
  , _optPostprocess :: ann -> [String] -> [String]
  }

renderFields :: Opts ann -> [PrettyField ann] -> [String]
renderFields opts fields = flattenBlocks blocks
  where
    len = maxNameLength 0 fields
    blocks = filter (not . null . _contentsBlock) -- empty blocks cause extra newlines #8236
           $ map (renderField' opts len) fields

    maxNameLength !acc []                            = acc
    maxNameLength !acc (PrettyField _ name _ : rest) = maxNameLength (max acc (BS.length name)) rest
    maxNameLength !acc (PrettySection {}   : rest)   = maxNameLength acc rest
    maxNameLength !acc (PrettyEmpty : rest) = maxNameLength acc rest

-- | Block of lines with flags for optional blank lines before and after
data Block = Block
  { _beforeBlock   :: Margin
  , _afterBlock    :: Margin
  , _contentsBlock :: [String]
  }

data Margin = Margin | NoMargin
  deriving Eq

-- | Collapse margins, any margin = margin
instance Semigroup Margin where
    NoMargin <> NoMargin = NoMargin
    _        <> _        = Margin

flattenBlocks :: [Block] -> [String]
flattenBlocks = go0 where
    go0 [] = []
    go0 (Block _before after strs : blocks) = strs ++ go after blocks

    go _surr' [] = []
    go  surr' (Block before after strs : blocks) = ins $ strs ++ go after blocks where
        ins | surr' <> before == Margin = ("" :)
            | otherwise                 = id

renderField :: Opts ann -> Int -> PrettyField ann -> Block
renderField (Opts rann indent post) fw (PrettyField ann name doc) =
    Block before after content
  where
    content = case comments of
      CommentBefore cs -> cs ++ post ann lines'
      CommentAfter  cs -> post ann lines' ++ cs
      NoComment        -> post ann lines'
    comments = rann ann
    before = case comments of
      CommentBefore [] -> NoMargin
      CommentAfter  [] -> NoMargin
      NoComment        -> NoMargin
      _                -> Margin

    (lines', after) = case lines narrow of
        []           -> ([ name' ++ ":" ], NoMargin)
        [singleLine] | length singleLine < 60
                     -> ([ name' ++ ": " ++ replicate (fw - length name') ' ' ++ narrow ], NoMargin)
        _            -> ((name' ++ ":") : map indent (lines (PP.render doc)), NoMargin)

    name' = fromUTF8BS name
    narrow = PP.renderStyle narrowStyle doc

    narrowStyle :: PP.Style
    narrowStyle = PP.style { PP.lineLength = PP.lineLength PP.style - fw }

renderField opts@(Opts rann indent post) _ (PrettySection ann name args fields) = Block NoMargin NoMargin $

    attachComments
      (post ann [ PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args ])
    ++
    map indent (renderFields opts fields)
  where
    attachComments content = case rann ann of
      CommentBefore cs -> cs ++ content
      CommentAfter  cs -> content ++ cs
      NoComment        -> content

renderField _ _ PrettyEmpty = Block NoMargin NoMargin mempty

renderField' :: Opts ann -> Int -> PrettyField ann -> Block
renderField' (Opts rann indent post) fw (PrettyField ann name doc) =
    Block before after content
  where
    content = case comments of
      CommentBefore cs -> cs ++ post ann lines'
      CommentAfter  cs -> post ann lines' ++ cs
      NoComment        -> post ann lines'
    comments = rann ann
    before = case comments of
      CommentBefore [] -> NoMargin
      CommentAfter  [] -> NoMargin
      NoComment        -> NoMargin
      _                -> Margin

    (lines', after) = case lines narrow of
        []           -> ([ name' ++ ":" ], NoMargin)
        _            ->
          bool
          ((name' ++ ":") : map indent (lines (PP.render doc)), NoMargin)
          ([name' ++ ": " ++ narrow], NoMargin)
          (name' == "cabal-version")
    name' = fromUTF8BS name
    narrow = PP.renderStyle narrowStyle doc

    narrowStyle :: PP.Style
    narrowStyle = PP.style { PP.lineLength = PP.lineLength PP.style - fw }

renderField' opts@(Opts rann indent post) _ (PrettySection ann name args fields) = Block Margin Margin $

    attachComments
      (post ann [ PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args ])
    ++
    map indent (renderFields opts fields)
  where
    attachComments content = case rann ann of
      CommentBefore cs -> cs ++ content
      CommentAfter  cs -> content ++ cs
      NoComment        -> content

renderField' _ _ PrettyEmpty = Block NoMargin NoMargin mempty

convertToFreeText :: [FieldLine Position] -> [FieldLine Position]
convertToFreeText [] = []
convertToFreeText ((FieldLine (Position r0 c0) bs0):xs) = [FieldLine (Position r0 c0) x]
  where
    x = mconcat $ snd $ foldl' (\(r',xs') (FieldLine (Position r _) bs) -> (r, xs' <> (replicate ((r-r')) ("\n")) <> [bs])) (r0,[bs0]) xs

ftField :: Field Position -> Field Position
ftField (Field n fs) = Field n (convertToFreeText fs)
ftField s@(Section _ _ _) = s

hasPrefixComma :: FieldLine ann -> Bool
hasPrefixComma (FieldLine _ bs) = C.isPrefixOf ", " bs

hasPrefixComma0 :: FieldLine ann -> Bool
hasPrefixComma0 (FieldLine _ bs) = not (C.isPrefixOf ", " bs) && (C.isPrefixOf "," bs)

hasPostfixComma :: FieldLine ann -> Bool
hasPostfixComma (FieldLine _ bs) = C.isSuffixOf "," bs

anyField :: (FieldLine ann -> Bool) -> Field ann -> Bool
anyField p (Field _ fs) = any p fs
anyField p (Section _ _ fss) = any (anyField p) fss

whichFields :: (FieldLine ann -> Bool) -> Field ann -> [Field ann]
whichFields p f@(Field _ fs) = bool [] [f] (any p fs)
whichFields p (Section _ _ fss) = foldMap (whichFields p) fss

stripComma :: FieldLine ann -> FieldLine ann
stripComma (FieldLine n bs) = FieldLine n
  (fromMaybe (fromMaybe bs (C.stripSuffix "," bs)) (C.stripPrefix ", " bs))

addPrefixComma :: FieldLine ann -> FieldLine ann
addPrefixComma (FieldLine n bs) = FieldLine n (", " <> bs)

prefixCommas :: (Field ann -> Bool) -> Field ann -> Field ann
prefixCommas p f@(Field n fs) = bool f (Field n (addPrefixComma . stripComma <$> fs)) (p f)
prefixCommas p (Section n a fss) = Section n a (prefixCommas p <$> fss)

noCommas :: (Field ann -> Bool) -> Field ann -> Field ann
noCommas p f@(Field n fs) = bool f (Field n (stripComma <$> fs)) (p f)
noCommas p (Section n a fss) = Section n a (noCommas p <$> fss)

isBlankField :: Field ann -> Bool
isBlankField (Field _ fs) = null fs
isBlankField (Section _ _ fss) = null fss

blankFields :: Field ann -> [Field ann]
blankFields f@(Field _ _) = bool [] [f] (isBlankField f)
blankFields (Section _ _ fss) = filter isBlankField fss

removeFields :: (Field ann -> Bool) -> Field ann -> Maybe (Field ann)
removeFields p f@(Field _ _) = bool (Just f) Nothing (p f)
removeFields p (Section n a fss) = Just $ Section n a (catMaybes $ removeFields p <$> fss)

removeFieldss :: (Field ann -> Bool) -> [Field ann] -> [Field ann]
removeFieldss p fs = catMaybes (removeFields p <$> fs)

data AddPolicy =
  AddReplace |
  AddAppend |
  AddIfNotExisting
  deriving (Eq)

addField :: AddPolicy -> Field Position -> [Field Position] -> [Field Position]
addField p f fs = case p of
  AddReplace -> notsames <> [f]
  AddAppend -> fs <> [f]
  AddIfNotExisting -> bool fs (fs <> [f]) (null sames)
  where
    sames = filter ((name f==) . name) fs
    notsames = filter ((name f/=) . name) fs

addFieldLine :: AddPolicy -> FieldLine Position -> Field Position -> Field Position
addFieldLine p fl (Field n fls) = Field n fls'
  where
    fls' = case p of
      AddReplace -> notsames <> [fl]
      AddAppend -> fls <> [fl]
      AddIfNotExisting -> bool fls (fls <> [fl]) (null sames)
    sames = filter ((fieldLineBS fl==) . fieldLineBS) fls
    notsames = filter ((fieldLineBS fl/=) . fieldLineBS) fls
addFieldLine _ _ s@(Section _ _ _) = s

renderCabal :: [Field Position] -> String
renderCabal = showFields'' (const (CommentAfter [])) (const id) 2 . printFields

rerenderCabal :: ByteString -> ByteString
rerenderCabal bs = (C.pack . showFields'' fComment (const id) 4 . fmap (fmap (fmap C.unpack)) . printFields $ fs) <> C.unlines extras
  where
    (fs, extras) = parseFieldsAndComments bs
    fComment [] = NoComment
    fComment xs = CommentBefore xs

licenseF :: Field Position
licenseF = Field (Name (Position 0 1) "license") [FieldLine (Position 0 21) "BSD-3-Clause"]

licenseFileF :: Field Position
licenseFileF = Field (Name (Position 0 1) "license-file") [FieldLine (Position 0 21) "LICENSE"]

initialPackageChar :: Parser () Char
initialPackageChar =
  FP.satisfyAscii
    ( `C.elem`
        ( C.pack $
            ['a' .. 'z']
              <> ['A' .. 'Z']
              <> ['0' .. '9']
        )
    )

packageChar :: Parser () Char
packageChar =
  FP.satisfyAscii
    ( `C.elem`
        ( C.pack $
            ['a' .. 'z']
              <> ['A' .. 'Z']
              <> ['-']
              <> ['0' .. '9']
        )
    )

validName :: Parser () String
validName = (:) <$> initialPackageChar <*> FP.many packageChar

data Dep = Dep { dep :: ByteString, depRange :: Maybe DepRange } deriving (Show, Ord, Eq, Generic)

data DepRange = DepCaret ByteString | DepLower ByteString | DepRange ByteString ByteString | DepUpper ByteString deriving (Show, Eq, Ord, Generic)

printDepRange :: DepRange -> ByteString
printDepRange (DepCaret v) = "^>=" <> v
printDepRange (DepLower v) = ">=" <> v
printDepRange (DepUpper v) = "<" <> v
printDepRange (DepRange l u) = ">=" <> l <> " && " <> "<" <> u

printDep :: Dep -> ByteString
printDep (Dep d r) = C.intercalate " " ([d] <> maybeToList (printDepRange <$> r))

depP :: Parser () Dep
depP =
  Dep <$>
  (FP.optional prefixComma *>
   ws *>
   (FP.byteStringOf validName) <*
   ws) <*>
  FP.optional depRangeP <*
  FP.optional postfixComma

depRangeP :: Parser () DepRange
depRangeP =
  ws *>
  ((DepCaret <$> (caret *> ws *> version)) FP.<|>
   (DepRange <$>
     (lower *> ws *> version) <* ws <* amps <* ws <*>
     (upper *> ws *> version)) FP.<|>
    (DepLower <$> (lower *> ws *> version))
  )
  <* ws

prefixComma :: Parser () ()
prefixComma = $(FP.string ", ")

postfixComma :: Parser () ()
postfixComma = $(FP.string ",")

space :: Parser () ()
space = $(FP.string " ")

ws :: Parser () ()
ws = pure () <* FP.many space

caret :: Parser () ()
caret = $(FP.string "^>=")

lower :: Parser () ()
lower = $(FP.string ">=")
upper :: Parser () ()
upper = $(FP.string "<")

amps :: Parser () ()
amps = $(FP.string "&&")

versionChar :: Parser () Char
versionChar =
  FP.satisfyAscii
    ( `C.elem`
        ( C.pack $
              ['.']
              <> ['0' .. '9']
        )
    )

version :: Parser () ByteString
version = FP.byteStringOf (FP.some versionChar)

parseOK :: Parser e a -> ByteString -> Either ByteString a
parseOK p bs = case FP.runParser p bs of
  FP.OK a "" -> Right a
  _ -> Left bs

preferredDeps :: Map.Map ByteString ByteString
preferredDeps = Map.fromList $ ((\x -> (dep x,printDep x))) <$> [x | (Right x) <- parseOK depP <$> preferredDepsBS]

preferredDepsBS :: [ByteString]
preferredDepsBS =
  [
    "adjunctions >=4.0 && <5"
  , "algebraic-graphs >=0.6 && <0.8"
  , "base >=4.7 && <5"
  , "bifunctors >=5.5.11 && <5.7"
  , "box >=0.9 && <0.10"
  , "box-socket >=0.4 && <0.5"
  , "bytestring >=0.11.3 && <0.13"
  , "chart-svg >=0.4 && <0.5"
  , "containers >=0.6 && <0.7"
  , "deepseq >=1.4.4 && <1.5"
  , "distributive >=0.4 && <0.7"
  , "flatparse >=0.3.5 && <0.6"
  , "formatn >=0.2.1 && <0.4"
  , "lucid >=2.9 && <2.12"
  , "mealy >=0.4 && <0.5"
  , "mtl >=2.2.2 && <2.4"
  , "numhask >=0.10 && <0.12"
  , "numhask-array >=0.10 && <0.12"
  , "numhask-space >=0.10 && <0.12"
  , "optics-core >=0.4 && <0.5"
  , "optics-extra >=0.4 && <0.5"
  , "optparse-applicative >=0.17 && <0.19"
  , "perf >=0.10 && <0.12"
  , "pretty >=1.1.3 && <1.1.4"
  , "profunctors >=5.6.2 && <5.7"
  , "random >=1.2 && <1.3"
  , "rdtsc >=1.3 && <1.4"
  , "semigroupoids >=5.3 && <6.1"
  , "string-interpolate >=0.3 && <0.4"
  , "tasty >=1.2 && <1.5"
  , "tasty-golden >=2.3.1.1 && <2.4"
  , "tdigest >=0.2.1 && <0.4"
  , "template-haskell >=2.16 && <2.21"
  , "text >=1.2 && <2.1"
  , "these >=1.1 && <1.3"
  , "time >=1.9 && <1.13"
  , "tree-diff >=0.3 && <0.4"
  , "unordered-containers >=0.2 && <0.3"
  , "vector >=0.12.3 && <0.14"
  , "vector-algorithms >=0.8.0 && <0.10"
  , "web-rep >=0.10.1 && <0.11"
  ]

subPreferredDeps :: ByteString -> ByteString
subPreferredDeps bs =
  case parseOK depP bs of
    Left _ -> bs
    Right x -> fromMaybe (printDep x) (Map.lookup (dep x) preferredDeps)

-- | checks that the dep name is not the library itself, in which case it subs the rangeless dep.
subPreferredDeps' :: ByteString -> ByteString -> ByteString
subPreferredDeps' libbs bs =
  case parseOK depP bs of
    Left _ -> bs
    Right x ->
      bool (fromMaybe (printDep x) (Map.lookup (dep x) preferredDeps)) (dep x) (libbs == dep x)

fieldOrdering :: Map.Map ByteString Double
fieldOrdering = Map.fromList
  [ ("author",6)
  , ("bug-reports",7.6)
  , ("build-depends",3)
  , ("build-type",8)
  , ("cabal-version",0)
  , ("category",20)
  , ("copyright",20)
  , ("default-extensions",6)
  , ("default-language",1)
  , ("description",7.8)
  , ("exposed-modules",4)
  , ("extra-doc-files",9)
  , ("ghc-options",7)
  , ("homepage",7.5)
  , ("hs-source-dirs",2)
  , ("import",0)
  , ("license",4)
  , ("license-file",5)
  , ("location",12)
  , ("main-is",0.5)
  , ("maintainer",7)
  , ("name",1)
  , ("other-modules",5)
  , ("synopsis",7.7)
  , ("tested-with",8.5)
  , ("type",11)
  , ("version",2)
  , ("source-repository", 10)
  , ("common", 11)
  , ("library", 12)
  , ("executable",13)
  , ("test-suite",14)
  ]

sortFields :: [Field ann] -> [Field ann]
sortFields fs = List.sortOn (\f -> (fromMaybe 100 (Map.lookup (name f) fieldOrdering), name2 f)) (sortInnerFields <$> fs)

name2 :: Field ann -> Maybe ByteString
name2 (Field _ fl) = listToMaybe (fieldLineBS <$> fl)
name2 (Section _ a _) = listToMaybe $ snd . secName <$> a

sortInnerFields :: Field ann -> Field ann
sortInnerFields f@(Field _ _) = f
sortInnerFields (Section n a fss) = Section n a (sortFields $ sortInnerFields <$> fss)

fieldLineSorts :: [ByteString]
fieldLineSorts =
  [ "build-depends"
  , "exposed-modules"
  , "default-extensions"
  , "ghc-options"
  , "extra-doc-files"
  , "tested-with"
  ]

sortFieldLines :: Field ann -> Field ann
sortFieldLines f@(Field n fl) =
  Field n (bool fl (List.sortOn fieldLineBS fl) (List.elem (name f) fieldLineSorts))
sortFieldLines (Section n a fss) = Section n a (sortFieldLines <$> fss)

-- from CabalFmt.Comments

newtype Comments = Comments [BS.ByteString]
  deriving stock Show
  deriving newtype (Semigroup, Monoid)

unComments :: Comments -> [BS.ByteString]
unComments (Comments cs) = cs

nullComments :: Comments -> Bool
nullComments (Comments cs) = null cs

-------------------------------------------------------------------------------
-- Attach comments
-------------------------------------------------------------------------------

-- | Returns a 'Field' forest with comments attached.
--
-- * Comments are attached to the field after it.
-- * A glitch: comments "inside" the field are attached to the field after it.
-- * End-of-file comments are returned separately.
--
attachComments
    :: BS.ByteString        -- ^ source with comments
    -> [Field Position] -- ^ parsed source fields
    -> ([Field Comments], Comments)
attachComments input inputFields =
    (overAnn attach inputFields, endComments)
  where
    inputFieldsU :: [(FieldPath, Field Position)]
    inputFieldsU = fieldUniverseN inputFields

    comments :: [(Int, Comments)]
    comments = extractComments input

    comments' :: Map.Map FieldPath Comments
    comments' = Map.fromListWith (flip (<>))
        [ (path, cs)
        | (l, cs) <- comments
        , path <- toList (findPath fieldAnn l inputFieldsU)
        ]

    endComments :: Comments
    endComments = mconcat
        [ cs
        | (l, cs) <- comments
        , isNothing (findPath fieldAnn l inputFieldsU)
        ]

    attach :: FieldPath -> Position -> Comments
    attach fp _pos = fromMaybe mempty (Map.lookup fp comments')

comments'' :: [(Int,Comments)] -> [(FieldPath, Field Position)] -> Map.Map FieldPath Comments
comments'' comments inputFieldsU = Map.fromListWith (flip (<>))
        [ (path, cs)
        | (l, cs) <- comments
        , path <- toList (findPath fieldAnn l inputFieldsU)
        ]

overAnn :: forall a b. (FieldPath -> a -> b) -> [Field a] -> [Field b]
overAnn f = go' id where
    go :: (FieldPath -> FieldPath) -> Int -> Field a -> Field b
    go g i (Field (Name a name) fls) =
        Field (Name b name) (b <$$ fls)
      where
        b = f (g (Nth i End)) a

    go g i (Section (Name a name) args fls) =
        Section (Name b name) (b <$$ args) (go' (g . Nth i) fls)
      where
        b = f (g (Nth i End)) a

    go' :: (FieldPath -> FieldPath) -> [Field a] -> [Field b]
    go' g xs = zipWith (go g) [0..] xs

    (<$$) :: (Functor f, Functor g) => x -> f (g y) -> f (g x)
    x <$$ y = (x <$) <$> y

-------------------------------------------------------------------------------
-- Find comments in the input
-------------------------------------------------------------------------------

extractComments :: BS.ByteString -> [(Int, Comments)]
extractComments = go . zip [1..] . map (BS.dropWhile isSpace8) . C.lines where
    go :: [(Int, BS.ByteString)] -> [(Int, Comments)]
    go [] = []
    go ((n, bs) : rest)
        | isComment bs = case span ((isComment .|| BS.null) . snd) rest of
            (h,t) -> (n, Comments $ bs : map snd h) : go t
        | otherwise = go rest

    (f .|| g) x = f x || g x

    isSpace8 w = w == 9 || w == 32

    isComment :: BS.ByteString -> Bool
    isComment = BS.isPrefixOf "--"

-------------------------------------------------------------------------------
-- FieldPath
-------------------------------------------------------------------------------

-- | Paths input paths. Essentially a list of offsets. Own type ofr safety.
data FieldPath
    = End
    | Nth Int FieldPath -- nth field
  deriving (Eq, Ord, Show)

fieldPathSize :: FieldPath -> Int
fieldPathSize = go 0 where
    go !acc End = acc
    go !acc (Nth _ fp) = go (succ acc) fp

fieldUniverseN :: [Field ann] -> [(FieldPath, Field ann)]
fieldUniverseN = concat . zipWith g [0..] where
    g n f' = [ (Nth n p, f'') | (p, f'') <- fieldUniverse f' ]

fieldUniverse :: Field ann -> [(FieldPath, Field ann)]
fieldUniverse f@(Section _ _ fs) = (End,f) : concat (zipWith g [0..] fs) where
    g n f' = [ (Nth n p, f'') | (p, f'') <- fieldUniverse f' ]
fieldUniverse f@(Field _ _)      = [(End, f)]

-- note: fieldUniverse* should produce 'FieldPath's in increasing order
-- that helps
findPath :: (a -> Position) -> Int -> [(FieldPath, a)] -> Maybe FieldPath
findPath _ _ [] = Nothing
findPath f l [(p, x)]
    | Position k _ <- f x =
        if l < k then Just p else Nothing
findPath f l ((_, x) : rest@((p, x') : _))
    | Position k  _ <- f x
    , Position k' _ <- f x' =
        if k < l && l < k'
        then Just p
        else findPath f l rest

makePositionTree :: [Field Position] -> Map.Map Int ([Int], String)
makePositionTree fs = foldFss Map.empty [] fs
  where
    foldFss m cursor fs = fst $ foldl' stepFss (m,cursor<>[0]) fs
    stepFss (m,cursor) (Field (Name (Position c _) _) fls) =
      (foldFls (Map.insertWith (\_ o -> o) c (cursor,"fieldname") m) cursor fls, inc cursor)
    stepFss (m,cursor) (Section (Name (Position c _) _) sas fss) =
      (foldFss (foldSas (Map.insertWith (\_ o -> o) c (cursor, "sectionname") m) cursor sas) cursor fss, inc cursor)
    foldFls m c fls = fst $ foldl' stepFls (m,(c<>[0])) fls
    stepFls (m,cursor) (FieldLine (Position c _) _) = (Map.insertWith (\_ o -> o) c (cursor,"fieldline") m, inc cursor)
    foldSas m c sas = fst $ foldl' stepSas (m,(c<>[0])) (sectionArgAnn <$> sas)
    stepSas (m,cursor) (Position c _) = (Map.insertWith (\_ o -> o) c (cursor, "sectionarg") m, inc cursor)

    inc :: [Int] -> [Int]
    inc [] = []
    inc xs = reverse (1 + head (reverse xs):drop 1 (reverse xs))

connectIndexAction :: (([Int], String) -> Field ann -> Field ann) -> [Field ann] -> [Field ann]
connectIndexAction faction fs = checkFss [] <$> fs
  where
    checkFss cursor f@(Field _ fls) =
      foldl' (&) (faction (cursor,"fieldname") f) $
      (\i -> faction (cursor<>[i],"fieldline") ) <$> [0..(length fls-1)]
    checkFss cursor s@(Section _ sas fss) = s'
      where
        sName = faction (cursor,"sectionname") s
        sArgs = foldl' (&) sName $
          (\i -> faction (cursor<>[i], "sectionarg")) <$> [0..(length sas-1)]
        s' = let (Section n a _) = sArgs in Section n a ((checkFss cursor) <$> fss)



addComment :: Maybe ([Int], String) -> [ByteString] -> ([Field [ByteString]], [ByteString]) -> ([Field [ByteString]],[ByteString])
addComment Nothing cs (fs, extras) = (fs, extras<>cs)
addComment (Just (cursor,tag)) cs (fs, extras) = (addc cs cursor tag fs, extras)

addc :: [ByteString] -> [Int] -> String -> [Field [ByteString]] -> [Field [ByteString]]
addc comments [x] "fieldname" fs = take x fs <> [f'] <> drop (x+1) fs
  where
    (Field (Name cs n) fls) = (List.!!) fs x
    f' = Field (Name (cs <> comments) n) fls
addc comments [x] "sectionname" fs = take x fs <> [f'] <> drop (x+1) fs
  where
    (Section (Name cs n) a fss) = (List.!!) fs x
    f' = Section (Name (cs <> comments) n) a fss
addc comments [x,y] "fieldline" fs = take x fs <> [f'] <> drop (x+1) fs
  where
    (Field n fls) = (List.!!) fs x
    (FieldLine cs bs) = (List.!!) fls y
    fl' = FieldLine (cs <> comments) bs
    f' = Field n (take y fls <> [fl'] <> drop (y+1) fls)
addc comments [x,y] "sectionarg" fs = take x fs <> [f'] <> drop (x+1) fs
  where
    (Section n sas fss) = (List.!!) fs x
    sa' = (<>comments) <$> (List.!!) sas y
    f' = Section n (take y sas <> [sa'] <> drop (y+1) sas) fss
addc comments (x:xs) tag fs = take x fs <> [f'] <> drop (x+1) fs
  where
    (Section n a fss) = (List.!!) fs x
    f' = Section n a (addc comments xs tag fss)

{-
insertComment :: Map.Map Int ([Int], String) -> (Int, ByteString) -> ([Field [ByteString]], [ByteString]) -> ([Field [ByteString]], [ByteString])
insertComment pt (c, bs) (fs, extras) = undefined
  where
    loc' :: Maybe Int
    loc' = fst <$> ((List.!!) (Map.toList pt) <$> (List.findIndex (\e -> fst e>c) (Map.toList pt)))
    faction (is, tag) f = bool (addComment tag f) f ((Just $ Just i) == (Map.lookup <$> loc' <*> pure pt))
    addComment "fieldname" (Field (Name cs n) fs) = Field (Name (cs<>[c]) n) fs
    addComment "sectionname" (Section (Name cs n) a fss) = Section (Name (cs<>[c]) n) a fss


-}

parseFieldsAndComments :: ByteString -> ([Field [ByteString]], [ByteString])
parseFieldsAndComments bs  = foldl' (&) (fmap (fmap (const [])) fs,[]) (uncurry addComment <$> cfs)
  where
    fs = convertDescription <$> toFields bs
    cs = extractComments bs
    pt = Map.toList $ makePositionTree fs
    cfs = fmap (second unComments . first (fmap snd)) (first (fmap ((List.!!) pt) . ((\x -> List.findIndex (\e -> fst e>x) pt))) <$> cs)
