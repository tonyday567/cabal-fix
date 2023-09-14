{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module CabalFix
  ( getCabal,
    putCabal,
    rerenderFile,
    getCabals,
    catList,
    filterChangedEdit,
    RenderConfig (..),
    defaultRenderConfig,
    rerenderCabal,
    getTestCabals,
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
    printDeps,
    printDep,
    printDepRange,
    fieldOrdering,
    sortInnerFields,
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
    printFieldsComments,
    preferredDepsBS,
    upstreams,
  )
where

import Control.Category ((>>>))
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.Char
import Data.Foldable
import Data.Function
import Data.Functor.Identity
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.String.Interpolate
import Data.TreeDiff hiding (FieldName)
import Data.TreeDiff qualified as T
import Data.TreeDiff.OMap qualified as O
import Distribution.Fields
import Distribution.Fields.Field hiding (fieldUniverse)
import Distribution.Parsec.Position
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Version
import Distribution.Utils.Generic
import FlatParse.Basic (Parser)
import FlatParse.Basic qualified as FP
import GHC.Generics
import System.Directory
import Text.PrettyPrint qualified as PP
import Prelude
import CabalFix.FlatParse
import Algebra.Graph qualified as G
import Algebra.Graph.ToGraph qualified as ToGraph
import Data.Set qualified as Set

data RenderConfig = RenderConfig
  { -- fields that should be converted to free text
    freeTexts :: [ByteString],
    -- fields that should be removed
    removals :: [ByteString],
    overwrites :: [(ByteString, ByteString, AddPolicy)],
    fixCommas :: [(ByteString, CommaStyle)],
    sortFieldLines :: [ByteString],
    sortFields :: Bool,
    fixBuildDeps :: Bool,
    depAlignment :: DepAlignment,
    removeBlankFields :: Bool,
    valueAligned :: ValueAlignment,
    sectionMargin :: Margin,
    commentMargin :: Margin,
    narrowN :: Int,
    indentN :: Int,
    replaceCategory :: Bool,
    replaceCopyright :: Bool
  }
  deriving (Eq, Show)

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
  RenderConfig
    ["description"]
    []
    [ ("license", "BSD-3-Clause", AddReplace),
      ("license-file", "LICENSE", AddReplace)
    ]
    [ ("extra-doc-files", NoCommas),
      ("build-depends", PrefixCommas)
    ]
    defaultFieldLineSorts
    True
    True
    DepAligned
    True
    ValueUnaligned
    Margin
    NoMargin
    60
    4
    False
    False

data CommaStyle = PrefixCommas | PostfixCommas | FreeformCommas | NoCommas deriving (Eq, Show)

data AddPolicy
  = AddReplace
  | AddAppend
  | AddIfNotExisting
  deriving (Eq, Show)

fieldOrdering :: Map.Map ByteString Double
fieldOrdering =
  Map.fromList
    [ ("author", 6),
      ("bug-reports", 7.6),
      ("build-depends", 3),
      ("build-type", 8),
      ("cabal-version", 0),
      ("category", 5.5),
      ("copyright", 5.2),
      ("default-extensions", 6),
      ("default-language", 1),
      ("description", 7.8),
      ("exposed-modules", 4),
      ("extra-doc-files", 9),
      ("ghc-options", 7),
      ("homepage", 7.5),
      ("hs-source-dirs", 2),
      ("import", 0),
      ("license", 4),
      ("license-file", 5),
      ("location", 12),
      ("main-is", 0.5),
      ("maintainer", 7),
      ("name", 1),
      ("other-modules", 5),
      ("synopsis", 7.7),
      ("tested-with", 8.5),
      ("type", 11),
      ("version", 2),
      ("source-repository", 10),
      ("common", 11),
      ("library", 12),
      ("executable", 13),
      ("test-suite", 14)
    ]

defaultFieldLineSorts :: [ByteString]
defaultFieldLineSorts =
  [ "build-depends",
    "exposed-modules",
    "default-extensions",
    "ghc-options",
    "extra-doc-files",
    "tested-with"
  ]

preferredDepsBS :: [(ByteString, ByteString)]
preferredDepsBS =
  [ ("adjunctions", ">=4.0 && <5"),
    ("algebraic-graphs", ">=0.6 && <0.8"),
    ("base", ">=4.7 && <5"),
    ("bifunctors", ">=5.5.11 && <5.7"),
    ("box", ">=0.9 && <0.10"),
    ("box-socket", ">=0.4 && <0.5"),
    ("bytestring", ">=0.11.3 && <0.13"),
    ("chart-svg", ">=0.5 && <0.6"),
    ("containers", ">=0.6 && <0.7"),
    ("distributive", ">=0.4 && <0.7"),
    ("flatparse", ">=0.3.5 && <0.6"),
    ("formatn", ">=0.2.1 && <0.4"),
    ("mealy", ">=0.4 && <0.5"),
    ("mtl", ">=2.2.2 && <2.4"),
    ("numhask", ">=0.10 && <0.12"),
    ("numhask-array", ">=0.10 && <0.12"),
    ("numhask-space", ">=0.10 && <0.12"),
    ("optics-core", ">=0.4 && <0.5"),
    ("optics-extra", ">=0.4 && <0.5"),
    ("optparse-applicative", ">=0.17 && <0.19"),
    ("perf", ">=0.12 && <0.13"),
    ("pretty", ">=1.1.3 && <1.1.4"),
    ("profunctors", ">=5.6.2 && <5.7"),
    ("random", ">=1.2 && <1.3"),
    ("rdtsc", ">=1.3 && <1.4"),
    ("semigroupoids", ">=5.3 && <6.1"),
    ("string-interpolate", ">=0.3 && <0.4"),
    ("tasty", ">=1.2 && <1.6"),
    ("tasty-golden", ">=2.3.1.1 && <2.4"),
    ("tdigest", ">=0.2.1 && <0.4"),
    ("template-haskell", ">=2.16 && <2.21"),
    ("text", ">=1.2 && <2.2"),
    ("these", ">=1.1 && <1.3"),
    ("time", ">=1.9 && <1.13"),
    ("tree-diff", ">=0.3 && <0.4"),
    ("unordered-containers", ">=0.2 && <0.3"),
    ("vector", ">=0.12.3 && <0.14"),
    ("vector-algorithms", ">=0.8.0 && <0.10"),
    ("web-rep", ">=0.11 && <0.12")
  ]

renderCabal :: RenderConfig -> [Field Position] -> String
renderCabal rcfg = showFields'' rcfg (const (CommentAfter [])) (const id) 2 . printFields

rerenderCabal :: RenderConfig -> ByteString -> ByteString
rerenderCabal rcfg bs = (C.pack . showFields'' rcfg fComment (const id) (indentN rcfg) . fmap (fmap (fmap C.unpack)) . printFieldsComments $ fs') <> C.unlines extras
  where
    (fs, extras) = parseFieldsAndComments (freeTexts rcfg) bs
    -- position info is now gone
    (Field (Name _ _) (FieldLine _ libdep : _)) = head $ filter (hasName ["name"]) fs
    fs' =
      defaultSortFields $
        fmap (sortFieldLinesFor (sortFieldLines rcfg)) $
          bool id (fmap (fixBuildDeps' rcfg libdep)) (fixBuildDeps rcfg) $
            bool id (replaceCategory' libdep) (replaceCategory rcfg) $
              bool id (replaceCopyright' libdep) (replaceCopyright rcfg) $
                adds rcfg $
                  fmap (fixcommas' rcfg) $
                    bool id (filter (not . isBlankField)) (removeBlankFields rcfg) $
                      filter (not . hasName (removals rcfg)) fs
    fComment [] = NoComment
    fComment xs = CommentBefore xs

adds :: RenderConfig -> [Field [a]] -> [Field [a]]
adds rcfg x = foldl' (&) x $ overwrites rcfg & fmap (\(n, v, p) -> addField p (Field (Name [] n) [FieldLine [] v]))

fixcommas' :: RenderConfig -> Field ann -> Field ann
fixcommas' rcfg x = foldl' (&) x $ fixCommas rcfg & fmap (\(n, s) -> commas (hasName [n]) s)

fixBuildDeps' :: RenderConfig -> ByteString -> Field ann -> Field ann
fixBuildDeps' rcfg libdep (Field n@(Name _ "build-depends") fls) = Field n (fixBDLines libdep (depAlignment rcfg) fls)
fixBuildDeps' _ _ f@(Field _ _) = f
fixBuildDeps' rcfg libdep (Section n a fss) = Section n a (fixBuildDeps' rcfg libdep <$> fss)

fixBDLines :: ByteString -> DepAlignment -> [FieldLine ann] -> [FieldLine ann]
fixBDLines libdep align fls = fls'
  where
    deps = parseDepFL <$> fls
    pds = (", " <>) <$> printDepsPreferred libdep align deps
    fls' = zipWith setValueFL fls pds

replaceCategory' :: ByteString -> [Field [ByteString]] -> [Field [ByteString]]
replaceCategory' libdep fs = case mv of
  Nothing -> fs
  Just v -> addField AddReplace (Field (Name [] "category") [FieldLine [] v]) fs
  where
    mv = Map.lookup (C.unpack libdep) (Map.fromList catList)

replaceCopyright' :: ByteString -> [Field [ByteString]] -> [Field [ByteString]]
replaceCopyright' libdep fs = case mv of
  Nothing -> fs
  Just v -> addField AddReplace (Field (Name [] "copyright") [FieldLine [] v]) fs
  where
    mv = C.pack . ("Tony Day (c) " <>) . show <$> Map.lookup (C.unpack libdep) (Map.fromList yearList)

parseDepFL :: FieldLine ann -> Dep
parseDepFL (FieldLine _ fl) = uncurry Dep $ runParser_ depP fl

setValueFL :: FieldLine ann -> ByteString -> FieldLine ann
setValueFL (FieldLine ann _) = FieldLine ann

allCabalProjects :: FilePath -> IO [String]
allCabalProjects d = do
  cs <- getDirectoryContents d
  let cs' = filter (\x -> x /= "." && x /= "..") cs
  ps <- filterM (doesDirectoryExist . (\x -> path [d, x])) cs'
  filterM (doesFileExist . cabalPath d) ps

projectCabal :: FilePath -> String -> IO ByteString
projectCabal d p = BS.readFile (cabalPath d p)

getTestCabals :: IO [(FilePath, ByteString)]
getTestCabals =
  getDirectoryContents dir >>= (\fs ->
      mapM (BS.readFile . (dir <>)) fs & fmap (zip fs)) . filter (not . List.isPrefixOf ".")
  where
    dir = "/Users/tonyday/haskell/cabal-fix/test/cabals/"

getCabals :: FilePath -> IO [(FilePath, ByteString)]
getCabals dir =
  getDirectoryContents dir >>= (\fs ->
      mapM (BS.readFile . (dir <>)) fs & fmap (zip fs)) . filter (not . List.isPrefixOf ".")

getCabal :: FilePath -> IO ByteString
getCabal = BS.readFile

putCabal :: FilePath -> ByteString -> IO ()
putCabal = BS.writeFile

rerenderFile :: FilePath -> RenderConfig -> IO ()
rerenderFile fp cfg = do
  bs <- getCabal fp
  putCabal fp (rerenderCabal cfg bs)

allCabals :: FilePath -> IO [(String, ByteString)]
allCabals d = do
  ps <- allCabalProjects d
  mapM (\x -> (x,) <$> projectCabal d x) ps

readCabalFile :: FilePath -> IO [Field Position]
readCabalFile fp = do
  bs <- toContents fp
  pure $ toFields bs

toContents :: FilePath -> IO ByteString
toContents = BS.readFile

toFields :: ByteString -> [Field Position]
toFields bs = either (error . show) id $ readFields bs

writeCabalFile :: FilePath -> RenderConfig -> [Field Position] -> IO ()
writeCabalFile fp rcfg fs = writeFile fp (renderCabal rcfg fs)

cabals :: FilePath -> FilePath -> IO [String]
cabals h d = do
  cs <- getDirectoryContents (path [h, d])
  let cs' = filter (\x -> x /= "." && x /= "..") cs
  ps <- filterM (doesDirectoryExist . (\x -> path [h, d, x])) cs'
  filterM (doesFileExist . cabalPath (path [h, d])) ps

cabalPath :: FilePath -> FilePath -> FilePath
cabalPath d x = path [d, x, x <> ".cabal"]

path :: [FilePath] -> FilePath
path = List.intercalate "/"

-- | Unification of field and section names
name :: Field a -> ByteString
name (Field (Name _ n) _) = n
name (Section (Name _ n) _ _) = n

isSection :: Field a -> Bool
isSection (Field _ _) = False
isSection _ = True

fieldNames :: Field a -> [ByteString]
fieldNames (Field (Name _ n) _) = [n]
fieldNames (Section _ _ fss) = mconcat $ fieldNames <$> fss

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

count_ :: (Ord a) => [a] -> Map.Map a Int
count_ = foldl' (\x a -> Map.insertWith (+) a 1 x) Map.empty

collect_ :: (Ord k) => [(k, v)] -> Map.Map k [v]
collect_ = foldl' (\x (k, v) -> Map.insertWith (<>) k [v] x) Map.empty

-- | BSD3 clause from cabal init
licenseFile :: String -> String -> String
licenseFile a y =
  [i|Copyright (c) #{y}, #{a}

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
yearList = [("numhask", 2016), ("mealy", 2013), ("box", 2017), ("formatn", 2016), ("prettychart", 2023), ("code", 2023), ("poker-fold", 2020), ("numhask-space", 2016), ("iqfeed", 2014), ("box-socket", 2017), ("numhask-array", 2016), ("euler", 2023), ("tonyday567", 2020), ("foo", 2023), ("web-rep", 2015), ("dotparse", 2022), ("perf", 2018), ("anal", 2023), ("research-hackage", 2022), ("chart-svg", 2017), ("ephemeral", 2020)]

catList :: [(String, ByteString)]
catList =
  [ ("tonyday567", "web"),
    ("research-hackage", "project"),
    ("anal", "project"),
    ("numhask-array", "math"),
    ("chart-svg", "graphics"),
    ("cabal-fix", "distribution"),
    ("numhask-space", "math"),
    ("mealy", "algorithm"),
    ("formatn", "text"),
    ("prettychart", "graphcs"),
    ("dotparse", "graphics"),
    ("perf", "performance"),
    ("numhask", "math"),
    ("ephemeral", "machine learning"),
    ("box-socket", "web"),
    ("iqfeed", "API"),
    ("box", "control"),
    ("code", "project"),
    ("foo", "project"),
    ("web-rep", "web"),
    ("poker-fold", "games")
  ]

printFields :: [Field ann] -> [PrettyField ann]
printFields =
  runIdentity
    . genericFromParsecFields
      (Identity .: prettyFieldLines)
      (Identity .: prettySectionArgs)
  where
    (.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
    (f .: g) x y = f (g x y)

printFieldsComments :: [Field [ByteString]] -> [PrettyField [ByteString]]
printFieldsComments =
  runIdentity
    . genericFromParsecFields
      (Identity .: prettyFieldLines')
      (Identity .: prettySectionArgs')
  where
    (.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
    (f .: g) x y = f (g x y)

-- | Used in 'fromParsecFields'.
prettyFieldLines' :: FieldName -> [FieldLine [ByteString]] -> PP.Doc
prettyFieldLines' _ fls =
  PP.vcat $
    mconcat $
      [ PP.text . fromUTF8BS <$> cs <> [bs]
        | FieldLine cs bs <- fls
      ]

-- | Used in 'fromParsecFields'.
prettySectionArgs' :: FieldName -> [SectionArg [ByteString]] -> [PP.Doc]
prettySectionArgs' _ =
  fmap $
    mconcat . \case
      SecArgName cs bs -> showToken . fromUTF8BS <$> cs <> [bs]
      SecArgStr cs bs -> showToken . fromUTF8BS <$> cs <> [bs]
      SecArgOther cs bs -> PP.text . fromUTF8BS <$> cs <> [bs]

convertFreeText :: [ByteString] -> Field Position -> Field Position
convertFreeText freeTexts f@(Field n fls) = bool f (Field n (convertToFreeText fls)) (hasName freeTexts f)
convertFreeText freeTexts (Section n a fss) = Section n a (convertFreeTexts freeTexts fss)

convertFreeTexts :: [ByteString] -> [Field Position] -> [Field Position]
convertFreeTexts freeTexts fs = snd $ foldl' step (Nothing, []) fs
  where
    step :: (Maybe (Field Position), [Field Position]) -> Field Position -> (Maybe (Field Position), [Field Position])
    step (descFP, res) nextFP
      | isNothing descFP && hasName freeTexts nextFP = (Just $ convertFreeText freeTexts nextFP, res)
      | isNothing descFP && not (hasName freeTexts nextFP) = (Nothing, res <> [nextFP])
      | isJust descFP && hasName freeTexts nextFP = (Just $ convertFreeText freeTexts nextFP, res <> [descFP'])
      | isJust descFP && not (hasName freeTexts nextFP) = (Nothing, res <> [descFP', nextFP])
      where
        (Just (Field _ ((FieldLine (Position c0 _) _) : _))) = descFP
        (Just (Field n fls)) = descFP
        c1 = firstCol nextFP
        (FieldLine ann fls') = head fls
        descFP' = Field n [FieldLine ann (fls' <> C.pack (replicate (c1 - c0 - length (C.lines fls')) '\n'))]

hasName :: [ByteString] -> Field ann -> Bool
hasName freeTexts (Field (Name _ n) _) = n `elem` freeTexts
hasName _ _ = False

firstCol :: Field Position -> Int
firstCol (Field (Name (Position c _) _) _) = c
firstCol (Section (Name (Position c _) _) _ _) = c

descriptionPreserveNewlines :: FieldName -> [FieldLine Position] -> PP.Doc
descriptionPreserveNewlines "description" fs = prettyFieldLines "description" (convertToFreeText fs)
descriptionPreserveNewlines n fs = prettyFieldLines n fs

preserveNewlines :: [FieldLine Position] -> PP.Doc
preserveNewlines [] = PP.empty
preserveNewlines ((FieldLine (Position r0 _) bs0) : xs) = PP.vcat $ snd $ foldl' (\(r', xs') (FieldLine (Position r _) bs) -> (r, xs' <> replicate (r - r' - 1) (PP.text "\n") <> [PP.text $ fromUTF8BS bs])) (r0, [PP.text $ fromUTF8BS bs0]) xs

prettyFieldLines :: FieldName -> [FieldLine ann] -> PP.Doc
prettyFieldLines _ fls =
  PP.vcat
    [ PP.text $ fromUTF8BS bs
      | FieldLine _ bs <- fls
    ]

prettySectionArgs :: FieldName -> [SectionArg ann] -> [PP.Doc]
prettySectionArgs _ = fmap $ \case
  SecArgName _ bs -> showToken $ fromUTF8BS bs
  SecArgStr _ bs -> showToken $ fromUTF8BS bs
  SecArgOther _ bs -> PP.text $ fromUTF8BS bs

-- | 'showFields' with user specified indentation.
showFields'' ::
  RenderConfig ->
  -- | Convert an annotation to lined to preceed the field or section.
  (ann -> CommentPosition) ->
  -- | Post-process non-annotation produced lines.
  (ann -> [String] -> [String]) ->
  -- | Indentation level.
  Int ->
  -- | Fields/sections to show.
  [PrettyField ann] ->
  String
showFields'' rcfg rann post n = unlines . renderFields rcfg (Opts rann indent post)
  where
    -- few hardcoded, "unrolled"  variants.
    indent
      | n == 4 = indent4
      | n == 2 = indent2
      | otherwise = (replicate (max n 1) ' ' ++)

    indent4 :: String -> String
    indent4 [] = []
    indent4 xs = ' ' : ' ' : ' ' : ' ' : xs

    indent2 :: String -> String
    indent2 [] = []
    indent2 xs = ' ' : ' ' : xs

data Opts ann = Opts
  { _optAnnotation :: ann -> CommentPosition,
    _optIndent :: String -> String,
    _optPostprocess :: ann -> [String] -> [String]
  }

renderFields :: RenderConfig -> Opts ann -> [PrettyField ann] -> [String]
renderFields rcfg opts fields = flattenBlocks blocks
  where
    len = maxNameLength 0 fields
    blocks =
      filter (not . null . _contentsBlock) $ -- empty blocks cause extra newlines #8236
        map (renderField rcfg opts len) fields

    maxNameLength !acc [] = acc
    maxNameLength !acc (PrettyField _ name _ : rest) = maxNameLength (max acc (BS.length name)) rest
    maxNameLength !acc (PrettySection {} : rest) = maxNameLength acc rest
    maxNameLength !acc (PrettyEmpty : rest) = maxNameLength acc rest

-- | Block of lines with flags for optional blank lines before and after
data Block = Block
  { _beforeBlock :: Margin,
    _afterBlock :: Margin,
    _contentsBlock :: [String]
  }
  deriving (Show, Eq)

data Margin = Margin | NoMargin
  deriving (Eq, Show)

-- | Collapse margins, any margin = margin
instance Semigroup Margin where
  NoMargin <> NoMargin = NoMargin
  _ <> _ = Margin

flattenBlocks :: [Block] -> [String]
flattenBlocks = go0
  where
    go0 [] = []
    go0 (Block _before after strs : blocks) = strs ++ go after blocks

    go _surr' [] = []
    go surr' (Block before after strs : blocks) = ins $ strs ++ go after blocks
      where
        ins
          | surr' <> before == Margin = ("" :)
          | otherwise = id

data ValueAlignment = ValueAligned | ValueUnaligned deriving (Eq, Show)

lines_ :: String -> [String]
lines_ [] = []
lines_ s = lines s <> bool [] [""] ((== '\n') . last $ s)

renderField :: RenderConfig -> Opts ann -> Int -> PrettyField ann -> Block
renderField rcfg (Opts rann indent post) fw (PrettyField ann name doc) =
  Block before after content
  where
    content = case comments of
      CommentBefore cs -> cs ++ post ann lines'
      CommentAfter cs -> post ann lines' ++ cs
      NoComment -> post ann lines'
    comments = rann ann
    before = case comments of
      CommentBefore [] -> NoMargin
      CommentAfter [] -> NoMargin
      NoComment -> NoMargin
      _ -> commentMargin rcfg

    (lines', after) = case lines_ narrow of
      [] -> ([name' ++ ":"], NoMargin)
      [singleLine]
        | length singleLine < narrowN rcfg ->
            ([name' ++ ": " ++ replicate (bool 0 (fw - length name') (valueAligned rcfg == ValueAligned)) ' ' ++ narrow], NoMargin)
      _ -> ((name' ++ ":") : map indent (lines_ (PP.render doc)), NoMargin)

    name' = fromUTF8BS name
    narrow = PP.renderStyle narrowStyle doc

    narrowStyle :: PP.Style
    narrowStyle = PP.style {PP.lineLength = PP.lineLength PP.style - fw}
renderField rcfg opts@(Opts rann indent post) _ (PrettySection ann name args fields) =
  Block (sectionMargin rcfg) (sectionMargin rcfg) $
    attachComments
      (post ann [PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args])
      ++ map indent (renderFields rcfg opts fields)
  where
    attachComments content = case rann ann of
      CommentBefore cs -> cs ++ content
      CommentAfter cs -> content ++ cs
      NoComment -> content
renderField _ _ _ PrettyEmpty = Block NoMargin NoMargin mempty

convertToFreeText :: [FieldLine Position] -> [FieldLine Position]
convertToFreeText [] = []
convertToFreeText ((FieldLine (Position r0 c0) bs0) : xs) = [FieldLine (Position r0 c0) x]
  where
    x = mconcat $ snd $ foldl' (\(r', xs') (FieldLine (Position r _) bs) -> (r, xs' <> replicate (r - r') "\n" <> [bs])) (r0, [bs0]) xs

ftField :: Field Position -> Field Position
ftField (Field n fs) = Field n (convertToFreeText fs)
ftField s@(Section {}) = s

hasPrefixComma :: FieldLine ann -> Bool
hasPrefixComma (FieldLine _ bs) = C.isPrefixOf ", " bs

hasPrefixComma0 :: FieldLine ann -> Bool
hasPrefixComma0 (FieldLine _ bs) = not (C.isPrefixOf ", " bs) && C.isPrefixOf "," bs

hasPostfixComma :: FieldLine ann -> Bool
hasPostfixComma (FieldLine _ bs) = C.isSuffixOf "," bs

anyField :: (FieldLine ann -> Bool) -> Field ann -> Bool
anyField p (Field _ fs) = any p fs
anyField p (Section _ _ fss) = any (anyField p) fss

whichFields :: (FieldLine ann -> Bool) -> Field ann -> [Field ann]
whichFields p f@(Field _ fs) = bool [] [f] (any p fs)
whichFields p (Section _ _ fss) = foldMap (whichFields p) fss

stripComma :: FieldLine ann -> FieldLine ann
stripComma (FieldLine n bs) =
  FieldLine
    n
    (fromMaybe (fromMaybe bs (C.stripSuffix "," bs)) (C.stripPrefix ", " bs))

addPrefixComma :: FieldLine ann -> FieldLine ann
addPrefixComma (FieldLine n bs) = FieldLine n (", " <> bs)

addPostfixComma :: FieldLine ann -> FieldLine ann
addPostfixComma (FieldLine n bs) = FieldLine n (bs <> ",")

commas :: (Field ann -> Bool) -> CommaStyle -> Field ann -> Field ann
commas p NoCommas f = noCommas p f
commas p PrefixCommas f = prefixCommas p f
commas p PostfixCommas f = postfixCommas p f
commas _ FreeformCommas f = f

prefixCommas :: (Field ann -> Bool) -> Field ann -> Field ann
prefixCommas p f@(Field n fs) = bool f (Field n (addPrefixComma . stripComma <$> fs)) (p f)
prefixCommas p (Section n a fss) = Section n a (prefixCommas p <$> fss)

postfixCommas :: (Field ann -> Bool) -> Field ann -> Field ann
postfixCommas p f@(Field n fs) = bool f (Field n (addPostfixComma . stripComma <$> fs)) (p f)
postfixCommas p (Section n a fss) = Section n a (postfixCommas p <$> fss)

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
removeFields p (Section n a fss) = Just $ Section n a (mapMaybe (removeFields p) fss)

removeFieldss :: (Field ann -> Bool) -> [Field ann] -> [Field ann]
removeFieldss p = mapMaybe (removeFields p)

addField :: AddPolicy -> Field ann -> [Field ann] -> [Field ann]
addField p f fs = case p of
  AddReplace -> notsames <> [f]
  AddAppend -> fs <> [f]
  AddIfNotExisting -> bool fs (fs <> [f]) (null sames)
  where
    sames = filter ((name f ==) . name) fs
    notsames = filter ((name f /=) . name) fs

addFieldLine :: AddPolicy -> FieldLine Position -> Field Position -> Field Position
addFieldLine p fl (Field n fls) = Field n fls'
  where
    fls' = case p of
      AddReplace -> notsames <> [fl]
      AddAppend -> fls <> [fl]
      AddIfNotExisting -> bool fls (fls <> [fl]) (null sames)
    sames = filter ((fieldLineBS fl ==) . fieldLineBS) fls
    notsames = filter ((fieldLineBS fl /=) . fieldLineBS) fls
addFieldLine _ _ s@(Section {}) = s

licenseF :: Field Position
licenseF = Field (Name (Position 0 1) "license") [FieldLine (Position 0 21) "BSD-3-Clause"]

licenseFileF :: Field Position
licenseFileF = Field (Name (Position 0 1) "license-file") [FieldLine (Position 0 21) "LICENSE"]


data Dep = Dep {dep :: ByteString, depRange :: ByteString} deriving (Show, Ord, Eq, Generic)

normDepRange :: ByteString -> ByteString
normDepRange dr = (maybe dr (C.pack . show . pretty) . (simpleParsecBS :: ByteString -> Maybe VersionRange)) dr

printDepRange :: VersionRange -> ByteString
printDepRange vr = C.pack . show . pretty $ vr

printDep :: Int -> Dep -> ByteString
printDep n (Dep d r) = C.intercalate (C.pack $ replicate n ' ') ([d] <> [normDepRange r])

data DepAlignment = DepAligned | DepUnaligned deriving (Eq, Show)

printDeps :: DepAlignment -> [Dep] -> [ByteString]
printDeps DepUnaligned ds = printDep 1 <$> ds
printDeps DepAligned ds = zipWith printDep ns ds
  where
    ls = BS.length . dep <$> ds
    ns = (\x -> maximum ls - x + 1) <$> ls

printDepPreferred :: ByteString -> Int -> Dep -> ByteString
printDepPreferred libd n (Dep d r) = C.intercalate (C.pack $ replicate n ' ') ([d] <> r')
  where
    r' = bool (maybe [normDepRange r] (: []) (Map.lookup d (Map.fromList preferredDepsBS))) [] (libd == d)

printDepsPreferred :: ByteString -> DepAlignment -> [Dep] -> [ByteString]
printDepsPreferred libd DepUnaligned ds = printDepPreferred libd 1 <$> ds
printDepsPreferred libd DepAligned ds = zipWith (printDepPreferred libd) ns ds
  where
    ls = BS.length . dep <$> ds
    ns = (\x -> maximum ls - x + 1) <$> ls

defaultSortFields :: [Field ann] -> [Field ann]
defaultSortFields fs = List.sortOn (\f -> (fromMaybe 100 (Map.lookup (name f) fieldOrdering), name2 f)) (sortInnerFields <$> fs)

name2 :: Field ann -> Maybe ByteString
name2 (Field _ fl) = listToMaybe (fieldLineBS <$> fl)
name2 (Section _ a _) = listToMaybe $ snd . secName <$> a

sortInnerFields :: Field ann -> Field ann
sortInnerFields f@(Field _ _) = f
sortInnerFields (Section n a fss) = Section n a (defaultSortFields $ sortInnerFields <$> fss)

sortFieldLinesFor :: [ByteString] -> Field ann -> Field ann
sortFieldLinesFor ns f@(Field n fl) =
  Field n (bool fl (List.sortOn fieldLineBS fl) (name f `elem` ns))
sortFieldLinesFor ns (Section n a fss) = Section n a (sortFieldLinesFor ns <$> fss)

-- from CabalFmt.Comments

newtype Comments = Comments [BS.ByteString]
  deriving stock (Show)
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
attachComments ::
  -- | source with comments
  BS.ByteString ->
  -- | parsed source fields
  [Field Position] ->
  ([Field Comments], Comments)
attachComments input inputFields =
  (overAnn attach inputFields, endComments)
  where
    inputFieldsU :: [(FieldPath, Field Position)]
    inputFieldsU = fieldUniverseN inputFields

    comments :: [(Int, Comments)]
    comments = extractComments input

    comments' :: Map.Map FieldPath Comments
    comments' =
      Map.fromListWith
        (flip (<>))
        [ (path, cs)
          | (l, cs) <- comments,
            path <- toList (findPath fieldAnn l inputFieldsU)
        ]

    endComments :: Comments
    endComments =
      mconcat
        [ cs
          | (l, cs) <- comments,
            isNothing (findPath fieldAnn l inputFieldsU)
        ]

    attach :: FieldPath -> Position -> Comments
    attach fp _pos = fromMaybe mempty (Map.lookup fp comments')

comments'' :: [(Int, Comments)] -> [(FieldPath, Field Position)] -> Map.Map FieldPath Comments
comments'' comments inputFieldsU =
  Map.fromListWith
    (flip (<>))
    [ (path, cs)
      | (l, cs) <- comments,
        path <- toList (findPath fieldAnn l inputFieldsU)
    ]

overAnn :: forall a b. (FieldPath -> a -> b) -> [Field a] -> [Field b]
overAnn f = go' id
  where
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

    (<$$) :: (Functor f, Functor g) => x -> f (g y) -> f (g x)
    x <$$ y = (x <$) <$> y
    go' g = zipWith (go g) [0 ..]

-------------------------------------------------------------------------------
-- Find comments in the input
-------------------------------------------------------------------------------

extractComments :: BS.ByteString -> [(Int, Comments)]
extractComments = go . zip [1 ..] . map (BS.dropWhile isSpace8) . C.lines
  where
    go :: [(Int, BS.ByteString)] -> [(Int, Comments)]
    go [] = []
    go ((n, bs) : rest)
      | isComment bs = case span ((isComment .|| BS.null) . snd) rest of
          (h, t) -> (n, Comments $ bs : map snd h) : go t
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
fieldPathSize = go 0
  where
    go !acc End = acc
    go !acc (Nth _ fp) = go (succ acc) fp

fieldUniverseN :: [Field ann] -> [(FieldPath, Field ann)]
fieldUniverseN = concat . zipWith g [0 ..]
  where
    g n f' = [(Nth n p, f'') | (p, f'') <- fieldUniverse f']

fieldUniverse :: Field ann -> [(FieldPath, Field ann)]
fieldUniverse f@(Section _ _ fs) = (End, f) : concat (zipWith g [0 ..] fs)
  where
    g n f' = [(Nth n p, f'') | (p, f'') <- fieldUniverse f']
fieldUniverse f@(Field _ _) = [(End, f)]

-- note: fieldUniverse* should produce 'FieldPath's in increasing order
-- that helps
findPath :: (a -> Position) -> Int -> [(FieldPath, a)] -> Maybe FieldPath
findPath _ _ [] = Nothing
findPath f l [(p, x)]
  | Position k _ <- f x =
      if l < k then Just p else Nothing
findPath f l ((_, x) : rest@((p, x') : _))
  | Position k _ <- f x,
    Position k' _ <- f x' =
      if k < l && l < k'
        then Just p
        else findPath f l rest

makePositionTree :: [Field Position] -> Map.Map Int ([Int], String)
makePositionTree fs = foldFss Map.empty [] fs
  where
    foldFss m cursor fs = fst $ foldl' stepFss (m, cursor <> [0]) fs
    stepFss (m, cursor) (Field (Name (Position c _) _) fls) =
      (foldFls (Map.insertWith (\_ o -> o) c (cursor, "fieldname") m) cursor fls, inc cursor)
    stepFss (m, cursor) (Section (Name (Position c _) _) sas fss) =
      (foldFss (foldSas (Map.insertWith (\_ o -> o) c (cursor, "sectionname") m) cursor sas) cursor fss, inc cursor)
    foldFls m c fls = fst $ foldl' stepFls (m, c <> [0]) fls
    stepFls (m, cursor) (FieldLine (Position c _) _) = (Map.insertWith (\_ o -> o) c (cursor, "fieldline") m, inc cursor)
    foldSas m c sas = fst $ foldl' stepSas (m, c <> [0]) (sectionArgAnn <$> sas)
    stepSas (m, cursor) (Position c _) = (Map.insertWith (\_ o -> o) c (cursor, "sectionarg") m, inc cursor)

    inc :: [Int] -> [Int]
    inc [] = []
    inc xs = reverse (1 + last xs : drop 1 (reverse xs))

connectIndexAction :: (([Int], String) -> Field ann -> Field ann) -> [Field ann] -> [Field ann]
connectIndexAction faction fs = checkFss [] <$> fs
  where
    checkFss cursor f@(Field _ fls) =
      foldl' (&) (faction (cursor, "fieldname") f) $
        (\i -> faction (cursor <> [i], "fieldline")) <$> [0 .. (length fls - 1)]
    checkFss cursor s@(Section _ sas fss) = s'
      where
        sName = faction (cursor, "sectionname") s
        sArgs =
          foldl' (&) sName $
            (\i -> faction (cursor <> [i], "sectionarg")) <$> [0 .. (length sas - 1)]
        s' = let (Section n a _) = sArgs in Section n a (checkFss cursor <$> fss)

addComment :: Maybe ([Int], String) -> [ByteString] -> ([Field [ByteString]], [ByteString]) -> ([Field [ByteString]], [ByteString])
addComment Nothing cs (fs, extras) = (fs, extras <> cs)
addComment (Just (cursor, tag)) cs (fs, extras) = (addc cs cursor tag fs, extras)

addc :: [ByteString] -> [Int] -> String -> [Field [ByteString]] -> [Field [ByteString]]
addc comments [x] "fieldname" fs = take x fs <> [f'] <> drop (x + 1) fs
  where
    (Field (Name cs n) fls) = (List.!!) fs x
    f' = Field (Name (cs <> comments) n) fls
addc comments [x] "sectionname" fs = take x fs <> [f'] <> drop (x + 1) fs
  where
    (Section (Name cs n) a fss) = (List.!!) fs x
    f' = Section (Name (cs <> comments) n) a fss
addc comments [x, y] "fieldline" fs = take x fs <> [f'] <> drop (x + 1) fs
  where
    (Field n fls) = (List.!!) fs x
    (FieldLine cs bs) = (List.!!) fls y
    fl' = FieldLine (cs <> comments) bs
    f' = Field n (take y fls <> [fl'] <> drop (y + 1) fls)
addc comments [x, y] "sectionarg" fs = take x fs <> [f'] <> drop (x + 1) fs
  where
    (Section n sas fss) = (List.!!) fs x
    sa' = (<> comments) <$> (List.!!) sas y
    f' = Section n (take y sas <> [sa'] <> drop (y + 1) sas) fss
addc comments (x : xs) tag fs = take x fs <> [f'] <> drop (x + 1) fs
  where
    (Section n a fss) = (List.!!) fs x
    f' = Section n a (addc comments xs tag fss)

parseFieldsAndComments :: [ByteString] -> ByteString -> ([Field [ByteString]], [ByteString])
parseFieldsAndComments freeTexts bs = foldl' (&) (fmap (fmap (const [])) fs, []) (uncurry addComment <$> cfs)
  where
    fs = convertFreeTexts freeTexts (toFields bs)
    cs = extractComments bs
    pt = Map.toList $ makePositionTree fs
    cfs = fmap (bimap (fmap snd) unComments) (first (fmap (pt List.!!) . (\x -> List.findIndex (\e -> fst e > x) pt)) <$> cs)

isUnchangedList :: [Edit EditExpr] -> Bool
isUnchangedList xs = all isCpy xs && all isUnchangedExpr (mapMaybe cpy xs)

isCpy :: Edit a -> Bool
isCpy (Cpy _) = True
isCpy _ = False

cpy :: Edit a -> Maybe a
cpy (Cpy a) = Just a
cpy _ = Nothing

isUnchangedEdit :: Edit EditExpr -> Bool
isUnchangedEdit (Cpy e) = isUnchangedExpr e
isUnchangedEdit _ = False

isUnchangedExpr :: EditExpr -> Bool
isUnchangedExpr e = isUnchangedList $ getList e

getList :: EditExpr -> [Edit EditExpr]
getList (EditApp _ xs) = xs
getList (EditRec _ m) = snd <$> O.toList m
getList (EditLst xs) = xs
getList (EditExp _) = []

filterChangedExprs :: EditExpr -> Maybe EditExpr
filterChangedExprs (EditApp n xs) =
  case filter (not . isUnchangedEdit) (filterChangedEdits xs) of
    [] -> Nothing
    xs' -> Just $ EditApp n xs'
filterChangedExprs (EditRec n m) =
  case filterChangedEditMap (O.fromList $ filter (not . isUnchangedEdit . snd) (O.toList m)) of
    Nothing -> Nothing
    Just m' -> Just (EditRec n m')
filterChangedExprs (EditLst xs) =
  case filter (not . isUnchangedEdit) (filterChangedEdits xs) of
    [] -> Nothing
    xs' -> Just (EditLst xs')
filterChangedExprs (EditExp _) = Nothing

filterChangedEdit :: Edit EditExpr -> Maybe (Edit EditExpr)
filterChangedEdit (Cpy a) = Cpy <$> filterChangedExprs a
filterChangedEdit x = Just x

filterChangedEdit' :: (f, Edit EditExpr) -> Maybe (f, Edit EditExpr)
filterChangedEdit' (f, e) = (f,) <$> filterChangedEdit e

filterChangedEdits :: [Edit EditExpr] -> [Edit EditExpr]
filterChangedEdits = mapMaybe filterChangedEdit

filterChangedEditMap :: O.OMap T.FieldName (Edit EditExpr) -> Maybe (O.OMap T.FieldName (Edit EditExpr))
filterChangedEditMap m = case xs' of
  [] -> Nothing
  xs'' -> Just $ O.fromList xs''
  where
    xs = O.toList m
    xs' = mapMaybe filterChangedEdit' xs

diffUpstreamSet :: (ToGraph.ToGraph t, Ord (ToGraph.ToVertex t)) => t -> Set.Set (ToGraph.ToVertex t) -> Set.Set (ToGraph.ToVertex t)
diffUpstreamSet g x = Set.difference (mconcat (fmap (`ToGraph.postSet` g) . toList $ x)) x

upstreams :: (ToGraph.ToGraph t, Ord (ToGraph.ToVertex t)) => ToGraph.ToVertex t -> t -> Set.Set (ToGraph.ToVertex t)
upstreams t g = go (t `ToGraph.postSet` g)
  where
    go s =
      let s' = diffUpstreamSet g s
       in bool (go (s <> s')) s (Set.empty == s')
