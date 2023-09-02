{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
    cabalsEmitter,
    allCabals',
    getTestCabals,
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
    cats,
    depOtherP,
    nota,
    untilP,
    depNameP,
    depPs',
    intercalated,
    lt,
    lte,
    gt,
    gte,
  )
where

import Box
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
import Distribution.Pretty
import Distribution.Utils.Generic
import FlatParse.Basic (Parser)
import FlatParse.Basic qualified as FP
import GHC.Generics
import System.Directory
import Text.PrettyPrint qualified as PP
import Prelude

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
    ("tasty", ">=1.2 && <1.5"),
    ("tasty-golden", ">=2.3.1.1 && <2.4"),
    ("tdigest", ">=0.2.1 && <0.4"),
    ("template-haskell", ">=2.16 && <2.21"),
    ("text", ">=1.2 && <2.1"),
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
                      filter (not . (hasName (removals rcfg))) $
                        fs
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
    mv = fmap (C.pack . ("Tony Day (c) " <>) . show) $ Map.lookup (C.unpack libdep) (Map.fromList yearList)

parseDepFL :: FieldLine ann -> Dep
parseDepFL (FieldLine _ fl) = parseDep fl

setValueFL :: FieldLine ann -> ByteString -> FieldLine ann
setValueFL (FieldLine ann _) bs = FieldLine ann bs

allCabalProjects :: FilePath -> IO [String]
allCabalProjects d = do
  cs <- getDirectoryContents d
  let cs' = filter (\x -> x /= "." && x /= "..") cs
  ps <- filterM (doesDirectoryExist . (\x -> path [d, x])) cs'
  ns <- filterM (doesFileExist . cabalPath d) ps
  pure ns

projectCabal :: FilePath -> String -> IO ByteString
projectCabal d p = BS.readFile (cabalPath d p)

getTestCabals :: IO [(FilePath, ByteString)]
getTestCabals =
  getDirectoryContents dir
    & fmap (filter (not . List.isPrefixOf "."))
    >>= \fs ->
      mapM (BS.readFile . (dir <>)) fs & fmap (zip fs)
  where
    dir = "/Users/tonyday/haskell/cabal-fix/test/cabals/"

getCabals :: FilePath -> IO [(FilePath, ByteString)]
getCabals dir =
  getDirectoryContents dir
    & fmap (filter (not . List.isPrefixOf "."))
    >>= \fs ->
      mapM (BS.readFile . (dir <>)) fs & fmap (zip fs)

getCabal :: FilePath -> IO ByteString
getCabal fp = BS.readFile fp

putCabal :: FilePath -> ByteString -> IO ()
putCabal fp bs = BS.writeFile fp bs

rerenderFile :: FilePath -> RenderConfig -> IO ()
rerenderFile fp cfg = do
  bs <- getCabal fp
  putCabal fp (rerenderCabal cfg bs)

allCabals :: FilePath -> IO [(String, ByteString)]
allCabals d = do
  ps <- allCabalProjects d
  mapM (\x -> (x,) <$> projectCabal d x) ps

-- cEmit :: FilePath -> CoEmitter IO (FilePath, String)
cabalsEmitter :: FilePath -> IO (CoEmitter IO (FilePath, String))
cabalsEmitter dir = do
  h <- getHomeDirectory
  ps <- cabals h dir
  pure $ witherE (fmap Just . (\x -> (x,) <$> readFile x) . cabalPath (path [h, dir])) <$> (qList ps)

allCabals' :: IO [(FilePath, String)]
allCabals' = do
  e <- cabalsEmitter "haskell"
  toListM <$|> e

fieldE :: Emitter IO (t, String) -> Emitter IO (t, [Field Position])
fieldE =
  witherE (\(fp, c) -> pure $ either (const Nothing) (Just . (fp,)) (readFields . C.pack $ c))

readCabalFile :: FilePath -> IO [Field Position]
readCabalFile fp = do
  bs <- toContents fp
  pure $ toFields bs

toContents :: FilePath -> IO ByteString
toContents fp = BS.readFile fp

toFields :: ByteString -> [Field Position]
toFields bs = either (error . show) id $ readFields bs

writeCabalFile :: FilePath -> RenderConfig -> [Field Position] -> IO ()
writeCabalFile fp rcfg fs = writeFile fp (renderCabal rcfg fs)

allFields :: IO [(FilePath, [Field Position])]
allFields = do
  e <- cabalsEmitter "haskell"
  toListM <$|> fieldE <$> e

cabals :: FilePath -> FilePath -> IO [String]
cabals h d = do
  cs <- getDirectoryContents (path [h, d])
  let cs' = filter (\x -> x /= "." && x /= "..") cs
  ps <- filterM (doesDirectoryExist . (\x -> path [h, d, x])) cs'
  ns <- filterM (doesFileExist . cabalPath (path [h, d])) ps
  pure ns

cabalPath :: FilePath -> FilePath -> FilePath
cabalPath d x = path [d, x, x <> ".cabal"]

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
    step :: (Maybe (Field Position), [Field Position]) -> (Field Position) -> (Maybe (Field Position), [Field Position])
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
        descFP' = Field n [FieldLine ann (fls' <> (C.pack $ replicate (c1 - c0 - length (C.lines fls')) '\n'))]

hasName :: [ByteString] -> Field ann -> Bool
hasName freeTexts (Field (Name _ n) _) = any (== n) freeTexts
hasName _ _ = False

firstCol :: Field Position -> Int
firstCol (Field (Name (Position c _) _) _) = c
firstCol (Section (Name (Position c _) _) _ _) = c

descriptionPreserveNewlines :: FieldName -> [FieldLine Position] -> PP.Doc
descriptionPreserveNewlines "description" fs = prettyFieldLines "description" (convertToFreeText fs)
descriptionPreserveNewlines n fs = prettyFieldLines n fs

preserveNewlines :: [FieldLine Position] -> PP.Doc
preserveNewlines [] = PP.empty
preserveNewlines ((FieldLine (Position r0 _) bs0) : xs) = PP.vcat $ snd $ foldl' (\(r', xs') (FieldLine (Position r _) bs) -> (r, xs' <> (replicate (r - r' - 1) (PP.text "\n")) <> [PP.text $ fromUTF8BS bs])) (r0, [PP.text $ fromUTF8BS bs0]) xs

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
lines_ [] = lines []
lines_ s = lines s <> bool [] [""] ((== '\n') . head . reverse $ s)

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
    x = mconcat $ snd $ foldl' (\(r', xs') (FieldLine (Position r _) bs) -> (r, xs' <> (replicate ((r - r')) ("\n")) <> [bs])) (r0, [bs0]) xs

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
removeFields p (Section n a fss) = Just $ Section n a (catMaybes $ removeFields p <$> fss)

removeFieldss :: (Field ann -> Bool) -> [Field ann] -> [Field ann]
removeFieldss p fs = catMaybes (removeFields p <$> fs)

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
addFieldLine _ _ s@(Section _ _ _) = s

licenseF :: Field Position
licenseF = Field (Name (Position 0 1) "license") [FieldLine (Position 0 21) "BSD-3-Clause"]

licenseFileF :: Field Position
licenseFileF = Field (Name (Position 0 1) "license-file") [FieldLine (Position 0 21) "LICENSE"]

initialPackageChar :: Parser e Char
initialPackageChar =
  FP.satisfyAscii
    ( `C.elem`
        ( C.pack $
            ['a' .. 'z']
              <> ['A' .. 'Z']
              <> ['0' .. '9']
        )
    )

packageChar :: Parser e Char
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

validName :: Parser e String
validName = (:) <$> initialPackageChar <*> FP.many packageChar

data Dep = Dep {dep :: ByteString, depRange :: Maybe DepRange} deriving (Show, Ord, Eq, Generic)

data DepBound = DepEq | DepCaret | DepStar | DepLTE | DepLT | DepGTE | DepGT deriving (Eq, Show)

data DepConnector = DepAnd | DepOr deriving (Eq, Show)

data DepSet = DepSet [Either (DepBound, Version) DepConnector] deriving (Show, Eq, Ord, Generic)

printDepRange :: DepRange -> ByteString
printDepRange (DepCaret v) = "^>=" <> v
printDepRange (DepLower v) = ">=" <> v
printDepRange (DepUpper v) = "<" <> v
printDepRange (DepRange l u) = ">=" <> l <> " && " <> "<" <> u

printDep :: Int -> Dep -> ByteString
printDep n (Dep d r) = C.intercalate (C.pack $ replicate n ' ') ([d] <> maybeToList (printDepRange <$> r))

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
    r' = bool (maybe (maybeToList $ printDepRange <$> r) (: []) (Map.lookup d (Map.fromList preferredDepsBS))) [] (libd == d)

printDepsPreferred :: ByteString -> DepAlignment -> [Dep] -> [ByteString]
printDepsPreferred libd DepUnaligned ds = printDepPreferred libd 1 <$> ds
printDepsPreferred libd DepAligned ds = zipWith (printDepPreferred libd) ns ds
  where
    ls = BS.length . dep <$> ds
    ns = (\x -> maximum ls - x + 1) <$> ls

depP :: Parser e Dep
depP =
  Dep
    <$> ( FP.optional prefixComma
            *> ws
            *> (FP.byteStringOf validName)
            <* ws
        )
    <*> FP.optional depRangeP
    <* FP.optional postfixComma

depP' :: Parser e Dep'
depP' =
  Dep'
    <$> (
            ws
            *> (FP.byteStringOf validName)
            <* ws
        )
    <*> FP.optional ((Left <$> depRangeP) FP.<|> (Right <$> nota ','))
    <* ws

depPs' :: Parser e [Dep']
depPs' =
  FP.optional (ws *> $(FP.char ',')) *>
  intercalated depP' $(FP.char ',')
  <* FP.optional (ws *> $(FP.char ','))

intercalated :: FP.Parser e item -> FP.Parser e sep -> FP.Parser e [item]
intercalated item sep =
  (:) <$> item <*> FP.chainr (:) (sep *> item) (pure [])

data Dep' = Dep' {dep' :: ByteString, depRange' :: Maybe (Either DepRange ByteString)} deriving (Show, Ord, Eq, Generic)

depOtherP :: Parser e (ByteString, ByteString)
depOtherP =
  (,) <$>
  (FP.optional prefixComma
            *> ws
            *> (FP.byteStringOf validName)
            <* ws
  )
  <*> (untilP ',')

depNameP :: Parser e ByteString
depNameP = either dep fst <$>
  (Left <$> depP) FP.<|>
  (Right <$> depOtherP)

nota :: Char -> FP.Parser e ByteString
nota c = FP.withSpan (FP.skipMany (FP.satisfy (/= c))) (\() s -> FP.unsafeSpanToByteString s)

untilP :: Char -> FP.Parser e ByteString
untilP c = nota c <* FP.satisfy (==c)

depRangeP :: Parser e DepRange
depRangeP =
  ws
    *> ( (DepCaret <$> (caret *> ws *> version))
           FP.<|> ( DepRange
                      <$> (gte *> ws *> version)
                      <* ws
                      <* amps
                      <* ws
                      <*> (lt *> ws *> version)
                  )
           FP.<|> (DepLower <$> (gte *> ws *> version))
       )
    <* ws

prefixComma :: Parser e ()
prefixComma = $(FP.string ", ")

postfixComma :: Parser e ()
postfixComma = $(FP.string ",")

space :: Parser e ()
space = $(FP.string " ")

ws :: Parser e ()
ws = pure () <* FP.many space

caret :: Parser e ()
caret = $(FP.string "^>=")

gte :: Parser e ()
gte = $(FP.string ">=")

gt :: Parser e ()
gt = $(FP.string ">")

lt :: Parser e ()
lt = $(FP.string "<")

lte :: Parser e ()
lte = $(FP.string "<=")

amps :: Parser e ()
amps = $(FP.string "&&")

versionChar :: Parser e Char
versionChar =
  FP.satisfyAscii
    ( `C.elem`
        ( C.pack $
            ['.']
              <> ['0' .. '9']
        )
    )

version :: Parser e ByteString
version = FP.byteStringOf (FP.some versionChar)

parseOK :: Parser e a -> ByteString -> Either ByteString a
parseOK p bs = case FP.runParser p bs of
  FP.OK a "" -> Right a
  _ -> Left bs

parseDep :: ByteString -> Dep
parseDep bs = either (error . C.unpack) id $ parseOK depP bs

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
  Field n (bool fl (List.sortOn fieldLineBS fl) (List.elem (name f) ns))
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
    go' g xs = zipWith (go g) [0 ..] xs

    (<$$) :: (Functor f, Functor g) => x -> f (g y) -> f (g x)
    x <$$ y = (x <$) <$> y

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
    foldFls m c fls = fst $ foldl' stepFls (m, (c <> [0])) fls
    stepFls (m, cursor) (FieldLine (Position c _) _) = (Map.insertWith (\_ o -> o) c (cursor, "fieldline") m, inc cursor)
    foldSas m c sas = fst $ foldl' stepSas (m, (c <> [0])) (sectionArgAnn <$> sas)
    stepSas (m, cursor) (Position c _) = (Map.insertWith (\_ o -> o) c (cursor, "sectionarg") m, inc cursor)

    inc :: [Int] -> [Int]
    inc [] = []
    inc xs = reverse (1 + head (reverse xs) : drop 1 (reverse xs))

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
        s' = let (Section n a _) = sArgs in Section n a ((checkFss cursor) <$> fss)

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

parseFieldsAndComments :: [ByteString] -> ByteString -> ([Field [ByteString]], [ByteString])
parseFieldsAndComments freeTexts bs = foldl' (&) (fmap (fmap (const [])) fs, []) (uncurry addComment <$> cfs)
  where
    fs = convertFreeTexts freeTexts (toFields bs)
    cs = extractComments bs
    pt = Map.toList $ makePositionTree fs
    cfs = fmap (second unComments . first (fmap snd)) (first (fmap ((List.!!) pt) . ((\x -> List.findIndex (\e -> fst e > x) pt))) <$> cs)

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
filterChangedEdits xs = mapMaybe filterChangedEdit xs

filterChangedEditMap :: O.OMap T.FieldName (Edit EditExpr) -> Maybe (O.OMap T.FieldName (Edit EditExpr))
filterChangedEditMap m = case xs' of
  [] -> Nothing
  xs'' -> Just $ O.fromList xs''
  where
    xs = O.toList m
    xs' = mapMaybe filterChangedEdit' xs

cats :: [(String, Int)]
cats =
  [ ("Accelerate", 27),
    ("Accessibility", 4),
    ("ACME", 55),
    ("Actors", 1),
    ("Adjunctions", 2),
    ("ADSB", 5),
    ("Aerospace", 9),
    ("Aeson", 4),
    ("AI", 79),
    ("Algebra", 58),
    ("Algebra Uniform", 1),
    ("Algorithm", 6),
    ("Algorithm Visualization", 2),
    ("Algorithmic Music Composition", 1),
    ("Algorithms", 134),
    ("Amazon", 1),
    ("Amqp", 1),
    ("Anatomy", 1),
    ("Animation", 6),
    ("ANSI", 1),
    ("AOP", 2),
    ("API", 40),
    ("Apple", 3),
    ("Application", 34),
    ("Application Server", 2),
    ("Applicative", 1),
    ("Archaeoinformatics", 1),
    ("Archive", 7),
    ("Argumentation", 4),
    ("Arithmetic", 1),
    ("Array", 13),
    ("Arrows", 9),
    ("Artificial Intelligence", 2),
    ("Arxiv", 1),
    ("ASP", 1),
    ("Aspect Oriented Programming", 3),
    ("Assembler", 1),
    ("AST", 1),
    ("Astrology", 1),
    ("Astronomy", 2),
    ("Atom", 1),
    ("ATS", 8),
    ("Attoparsec", 3),
    ("Attribute Grammars", 1),
    ("Audio", 15),
    ("Authentication", 14),
    ("Automatic Music Generation", 1),
    ("Automation", 3),
    ("Avers", 4),
    ("Aviation", 26),
    ("AWS", 384),
    ("Azure", 1),
    ("Backup", 3),
    ("Barcode", 1),
    ("Base", 1),
    ("Benchmark", 2),
    ("Benchmarking", 18),
    ("Big Data", 2),
    ("Binary", 5),
    ("Bindings", 64),
    ("Bio", 8),
    ("Bioinformatics", 129),
    ("Bit", 11),
    ("Bit Vectors", 13),
    ("Bitcoin", 19),
    ("Bits", 1),
    ("Blockchain", 4),
    ("Blog", 1),
    ("Book", 1),
    ("Browser", 8),
    ("BSD", 1),
    ("BSD3", 7),
    ("Bsparse", 1),
    ("Build", 10),
    ("Build Tool", 3),
    ("Builders", 1),
    ("Building", 6),
    ("Bundle", 2),
    ("Business", 8),
    ("Bytes", 1),
    ("ByteString", 6),
    ("ByteStrings", 1),
    ("BZip", 1),
    ("C", 6),
    ("C#", 1),
    ("Cabal", 4),
    ("Cache", 5),
    ("Caching", 1),
    ("CAPTCHA", 1),
    ("Cardano", 5),
    ("Cast", 1),
    ("Categories", 9),
    ("Category", 2),
    ("Catgirls", 1),
    ("CGI", 1),
    ("Chaos Music", 1),
    ("Charts", 6),
    ("Chat", 2),
    ("Chemistry", 11),
    ("CHs", 1),
    ("CI", 5),
    ("Classes", 1),
    ("Classification", 4),
    ("Clckwrks", 12),
    ("CLI", 50),
    ("CLI Tool", 5),
    ("Client", 8),
    ("Clipboard", 1),
    ("Closure", 1),
    ("Cloud", 227),
    ("Cloud Haskell", 5),
    ("CLR", 6),
    ("Clustering", 7),
    ("CmdLineArgs", 1),
    ("Code Competitions", 1),
    ("Code Generation", 30),
    ("Code-Generator", 1),
    ("Codec", 190),
    ("Codecs", 8),
    ("Codegen", 2),
    ("CodeQuality", 1),
    ("Coerce", 1),
    ("Combinatorics", 2),
    ("Combinators", 24),
    ("Command Line", 12),
    ("Command Line Tool", 5),
    ("Command Line Tools", 6),
    ("CommandLine", 4),
    ("Commerce", 1),
    ("Commercial", 2),
    ("Common-Parts", 1),
    ("Comonad", 4),
    ("Comonads", 17),
    ("Compatibility", 14),
    ("Compilation", 1),
    ("Compiler", 72),
    ("Compiler Plugin", 18),
    ("Compilers", 7),
    ("Compilers/Interpreters", 147),
    ("Composite", 9),
    ("Composition", 11),
    ("Compression", 33),
    ("Computational Geometry", 2),
    ("Computer Algebra", 1),
    ("Concourse", 1),
    ("Concurrency", 250),
    ("Concurrent", 13),
    ("Conduit", 76),
    ("Config", 2),
    ("Configuration", 65),
    ("Console", 96),
    ("Constraint", 1),
    ("Constraints", 24),
    ("Consul", 1),
    ("Containers", 7),
    ("Contract", 1),
    ("Contracts", 1),
    ("Contravariant", 5),
    ("Control", 818),
    ("Control Language", 1),
    ("Control.Monad", 1),
    ("Control.Parallel.Eden", 1),
    ("Convenience", 1),
    ("Conversion", 6),
    ("CouchDB", 1),
    ("CPP", 1),
    ("CRDT", 1),
    ("Criu", 2),
    ("Cron", 1),
    ("Crosswords", 1),
    ("Crypto", 30),
    ("Cryptocurrency", 1),
    ("Cryptography", 175),
    ("CSharp", 1),
    ("CsSyd", 1),
    ("CSV", 23),
    ("Culinary", 1),
    ("Cursor", 1),
    ("CustomSetup", 2),
    ("Data", 2427),
    ("Data Conduit", 1),
    ("Data Control", 2),
    ("Data Flow", 3),
    ("Data Mining", 28),
    ("Data Network", 1),
    ("Data Science", 8),
    ("Data Structure", 3),
    ("Data Structures", 325),
    ("Data Text JSON YAML Pandoc", 1),
    ("Data Text Uniform", 2),
    ("Data-structures", 1),
    ("Database", 539),
    ("Database Design", 1),
    ("Database Testing Web", 3),
    ("Databases", 3),
    ("Dataflow", 2),
    ("Datamining", 3),
    ("Date", 2),
    ("Debian", 5),
    ("Debug", 48),
    ("Debugging", 5),
    ("Decompiler", 1),
    ("Deep Learning", 2),
    ("Default", 2),
    ("Delay", 1),
    ("Demo", 7),
    ("Dependency Injection", 1),
    ("Dependent Types", 55),
    ("Derive-monoid", 1),
    ("Deriving", 1),
    ("Desktop", 20),
    ("Desktop Environment", 5),
    ("Development", 861),
    ("Development Web", 1),
    ("DevOps", 6),
    ("DFINITY", 1),
    ("Dhall", 10),
    ("Dhall Pandoc", 1),
    ("Diagnostics", 2),
    ("Diagram", 1),
    ("Diagrams", 1),
    ("Diffing", 1),
    ("Digest", 2),
    ("Dijkstra", 1),
    ("Directory", 2),
    ("Disassembler", 4),
    ("Distributed Computing", 62),
    ("Distributed Systems", 7),
    ("Distributed-Computing", 1),
    ("Distribution", 153),
    ("DNS", 2),
    ("Docker", 4),
    ("Documentation", 26),
    ("Domain Specific Language", 1),
    ("Download Manager", 2),
    ("DSL", 12),
    ("EBNF", 1),
    ("Eden", 3),
    ("Editing", 1),
    ("Editor", 21),
    ("Education", 33),
    ("Educational", 2),
    ("Effect", 23),
    ("Effectful", 1),
    ("Effects", 5),
    ("Efficient XML Pretty-printer", 1),
    ("Electrs", 1),
    ("Elliptic Curves", 1),
    ("Elm", 5),
    ("Emacs", 1),
    ("Email", 15),
    ("Embedded", 33),
    ("Enumerator", 33),
    ("Envars", 1),
    ("Environment", 2),
    ("Error", 1),
    ("Error Exception Uniform", 1),
    ("Error Handling", 26),
    ("Error Reporting", 1),
    ("Eternity", 1),
    ("Ethereum", 7),
    ("ETL", 1),
    ("Eval.so", 1),
    ("Event-sourcing", 1),
    ("Eventloop", 1),
    ("Eventsourcing", 13),
    ("Eventstore", 1),
    ("Example", 2),
    ("Exception", 2),
    ("Exceptions", 11),
    ("Executable", 2),
    ("Experiment", 2),
    ("Experimental", 3),
    ("Extension", 10),
    ("Faas", 1),
    ("Factory", 1),
    ("Factual", 2),
    ("Failure", 26),
    ("Fake", 2),
    ("FakeData", 2),
    ("Fay", 10),
    ("FCM", 1),
    ("Fedora", 3),
    ("Feed", 1),
    ("FFI", 139),
    ("FFI Tools", 10),
    ("File", 8),
    ("File Manager", 3),
    ("FilePath", 2),
    ("Filesystem", 38),
    ("Filter", 1),
    ("Finance", 81),
    ("Finance Network Bitcoin", 1),
    ("Financial", 2),
    ("Fitness", 1),
    ("Flatpak", 2),
    ("Flight", 1),
    ("Flink", 1),
    ("Folding", 2),
    ("Font", 3),
    ("Foreign", 114),
    ("Foreign Binding", 7),
    ("Formal Languages", 11),
    ("Formal Methods", 43),
    ("Format", 3),
    ("Formatting", 2),
    ("Foundation", 2),
    ("Fractals", 1),
    ("Framework", 5),
    ("FRP", 109),
    ("FSM", 1),
    ("Functions", 10),
    ("Functors", 9),
    ("Futhark", 5),
    ("Futures", 1),
    ("Game", 296),
    ("Game Engine", 41),
    ("Games", 6),
    ("Gateway", 1),
    ("Gemini", 5),
    ("Genealogy", 2),
    ("General", 4),
    ("Generative Music Grammars", 1),
    ("Generic", 5),
    ("Generics", 135),
    ("Gentoo", 1),
    ("Geo", 2),
    ("Geography", 15),
    ("Geometry", 18),
    ("Geospatial", 1),
    ("GHC", 33),
    ("GIS Programs", 1),
    ("Git", 15),
    ("GitHub", 5),
    ("GiveYouAHead", 2),
    ("Go", 1),
    ("Google", 188),
    ("Gps", 1),
    ("GPU", 2),
    ("Graph", 9),
    ("Graphics", 629),
    ("GraphQL", 12),
    ("Graphs", 44),
    ("Groundhog", 1),
    ("Groups", 2),
    ("GRPC", 1),
    ("GUI", 81),
    ("Hakyll", 3),
    ("HAM", 1),
    ("Ham-radio", 1),
    ("Happstack", 17),
    ("Hardware", 68),
    ("Hash", 9),
    ("Haskell", 16),
    ("Haskell Admin", 2),
    ("Haskell2010", 3),
    ("Haskell2020", 1),
    ("Haskell98", 2),
    ("Hasql", 20),
    ("Health", 2),
    ("Help", 2),
    ("Heuristics", 2),
    ("Hie", 2),
    ("HKD", 3),
    ("HLS", 2),
    ("HNum", 1),
    ("Hspec", 2),
    ("HTML", 26),
    ("HTTP", 7),
    ("Hxt", 1),
    ("Hydraulics", 1),
    ("Hydrology", 1),
    ("I18n", 2),
    ("I2C", 2),
    ("IDE", 17),
    ("Identification", 1),
    ("Image", 26),
    ("Image Processing", 2),
    ("Image Viewer", 3),
    ("Images", 1),
    ("In-other-words", 1),
    ("Indexed", 1),
    ("Infrastructure", 1),
    ("Inspection", 1),
    ("Instances", 1),
    ("Integration", 1),
    ("Interaction", 2),
    ("Interactive", 1),
    ("Interfaces", 8),
    ("Interpolation", 5),
    ("Interpreter", 2),
    ("Interpreters", 3),
    ("IO", 11),
    ("IO-Streams", 21),
    ("IoC", 1),
    ("IoT", 1),
    ("IRC", 12),
    ("IRC Client", 2),
    ("IRI", 1),
    ("Iteratee", 1),
    ("J", 1),
    ("Japanese Natural Language Processing", 1),
    ("Java", 15),
    ("JavaScript", 35),
    ("JSON", 114),
    ("JSON5", 1),
    ("JSX", 1),
    ("JVM", 16),
    ("Kerf", 1),
    ("Ketchup", 1),
    ("Keynote", 1),
    ("Keyword Extractor", 1),
    ("KML", 1),
    ("Korean", 1),
    ("Lalr", 1),
    ("Lambda Cube", 1),
    ("LambdaCalculus", 2),
    ("Language", 769),
    ("Language Tools", 1),
    ("Language.Nix", 2),
    ("Languages", 4),
    ("LaTeX", 6),
    ("Lazy", 1),
    ("Learning", 1),
    ("Learning Environments", 1),
    ("Learning Haskell", 1),
    ("Lens", 18),
    ("Lenses", 56),
    ("Lexer", 3),
    ("Lexers", 1),
    ("Lexing", 1),
    ("Lib", 3),
    ("Library", 58),
    ("Lightning", 3),
    ("Linear Algebra", 3),
    ("Linear Programming", 1),
    ("LinearAlgebra", 2),
    ("Linguistics", 6),
    ("Linnet", 3),
    ("Linux", 4),
    ("Linux Desktop", 1),
    ("Lisp", 1),
    ("List", 21),
    ("Little Game", 1),
    ("Live Coding", 7),
    ("Local Search", 2),
    ("Log", 1),
    ("Logger", 1),
    ("Logging", 62),
    ("Logic", 41),
    ("Logic Programming", 5),
    ("Logstash", 1),
    ("LruCache", 1),
    ("LUA", 1),
    ("Machine Learning", 69),
    ("Machine Vision", 3),
    ("Machine-learning", 1),
    ("Machines", 3),
    ("Macros", 1),
    ("Mail", 5),
    ("Managed Functions", 1),
    ("Manatee", 17),
    ("Map", 1),
    ("MapReduce", 2),
    ("Markdown", 2),
    ("Math", 724),
    ("Mathematics", 20),
    ("Maths", 8),
    ("Matrix", 1),
    ("Media", 21),
    ("Medical", 2),
    ("Megaparsec", 1),
    ("Memoization", 2),
    ("Memory", 5),
    ("Message-Oriented", 1),
    ("Message-Oriented Middleware", 5),
    ("Meta", 1),
    ("Metalanguage", 1),
    ("Metrics", 10),
    ("Microcontroller", 4),
    ("Microservice", 1),
    ("Middleware", 3),
    ("Minecraft", 2),
    ("Miscellaneous", 1),
    ("Miso", 2),
    ("Mit", 1),
    ("Mobile", 5),
    ("Model", 5),
    ("Modelling", 1),
    ("Modules", 1),
    ("Monad", 26),
    ("Monad Transformers", 1),
    ("Monadic Regions", 12),
    ("MonadIO", 1),
    ("Monads", 92),
    ("Money", 6),
    ("Monitoring", 13),
    ("Mptcp", 2),
    ("Mtl", 1),
    ("Multimedia", 3),
    ("Multimedia Player", 2),
    ("Mumeric.Statistics", 1),
    ("Murmur", 1),
    ("Music", 106),
    ("MusicBrainz", 1),
    ("Mutable State", 3),
    ("N2O", 4),
    ("NA", 1),
    ("Naqsha", 1),
    ("Natural Language", 2),
    ("Natural Language Processing", 92),
    ("Natural-language-processing", 1),
    ("Neovim", 8),
    ("Net", 3),
    ("Network", 1122),
    ("Network API", 1),
    ("Network APIs", 17),
    ("Network Control", 1),
    ("NetworkAPI", 1),
    ("NetworkAPIs", 1),
    ("Networking", 15),
    ("Nix", 35),
    ("NLP", 9),
    ("Noise", 2),
    ("Non-determinism", 2),
    ("None", 1),
    ("NonEmpty", 1),
    ("Ntrol", 1),
    ("Number Theory", 17),
    ("Numbers", 1),
    ("Numeric", 81),
    ("Numerical", 72),
    ("Numerics", 3),
    ("OAuth", 2),
    ("Object Storage", 1),
    ("Observability", 9),
    ("OCaml", 2),
    ("Ocilib", 1),
    ("ODPI-C", 1),
    ("Office", 1),
    ("OOP", 1),
    ("OpenAPI", 4),
    ("Opengl", 2),
    ("OpenLayers", 1),
    ("OpenTelemetry", 14),
    ("Operating System", 4),
    ("Operations", 1),
    ("Optics", 19),
    ("Optimisation", 21),
    ("Optimization", 23),
    ("Options", 15),
    ("Oracle", 2),
    ("Orphan Instances", 1),
    ("Other", 28),
    ("OverloadeLabels", 1),
    ("Package Management", 6),
    ("Package.Category", 1),
    ("Packaging", 2),
    ("PagerDuty", 1),
    ("Pandoc", 2),
    ("Parallel", 2),
    ("Parallelism", 38),
    ("Parry", 1),
    ("Parser", 28),
    ("Parser Builder", 1),
    ("Parser Combinators", 2),
    ("ParserCombinators", 1),
    ("Parsers", 7),
    ("Parsing", 257),
    ("Password", 6),
    ("Pattern", 4),
    ("Pattern Classification", 2),
    ("Pattern Recognition", 1),
    ("Payments", 4),
    ("PDF", 10),
    ("PDF Latex", 1),
    ("PDF Viewer", 1),
    ("Performance", 10),
    ("Permutations", 1),
    ("Persistent", 2),
    ("PersonalGrowth", 1),
    ("Phantom Types", 5),
    ("Phishing", 1),
    ("PHP", 1),
    ("Physics", 35),
    ("Picture", 1),
    ("Pinboard", 1),
    ("Pipes", 56),
    ("PL/SQL Tools", 1),
    ("Plaid", 1),
    ("Planning", 1),
    ("Plotting", 2),
    ("Plugin", 14),
    ("Plugins", 2),
    ("Poker", 3),
    ("Politic", 5),
    ("Polymorphism", 4),
    ("Polysemy", 16),
    ("Polysemy Filesystem", 2),
    ("Polysemy Vinyl", 1),
    ("Portal", 1),
    ("Possehl-Analytics", 7),
    ("PostgreSQL", 30),
    ("Potoki", 3),
    ("Prelude", 96),
    ("Preprocessor", 3),
    ("Presentation", 1),
    ("Pretty Printer", 24),
    ("Pretty-printing", 1),
    ("Primitive", 3),
    ("Probability", 1),
    ("Process Manager", 1),
    ("Productivity", 3),
    ("Profiling", 20),
    ("Profunctors", 4),
    ("Program", 7),
    ("Program Transformation", 2),
    ("Programming Uniform", 1),
    ("Project", 23),
    ("Project Management", 2),
    ("Prompt", 1),
    ("Propagators", 1),
    ("Proto", 1),
    ("Protocol", 16),
    ("Protocols", 1),
    ("Proxies", 1),
    ("PSP", 1),
    ("Ptr", 1),
    ("Pugs", 8),
    ("Pup-Events", 5),
    ("PureScript", 2),
    ("PVP", 1),
    ("Python", 1),
    ("QL", 1),
    ("QR", 2),
    ("QualifiedDo", 1),
    ("Quality", 1),
    ("Quant", 1),
    ("Quantum", 3),
    ("QuasiQuotes", 7),
    ("QuickCheck", 3),
    ("Quipper", 9),
    ("Qux", 2),
    ("Raaz", 1),
    ("Rabbitmq", 1),
    ("Radio", 1),
    ("RAKE", 1),
    ("Random", 20),
    ("Raphics", 1),
    ("Raspberrypi", 3),
    ("Raw", 2),
    ("RDF", 1),
    ("Reactive", 10),
    ("Reactivity", 39),
    ("Record", 1),
    ("Records", 38),
    ("Recursion", 11),
    ("Redis", 5),
    ("Refactoring", 6),
    ("Reflection", 11),
    ("Reflex", 6),
    ("Regex", 8),
    ("Relational Algebra", 1),
    ("Relaxng", 1),
    ("Relude", 1),
    ("Remote Management", 7),
    ("REPL", 2),
    ("Resources", 2),
    ("Reverse Engineering", 4),
    ("Rewriting", 2),
    ("RFC", 1),
    ("Robotics", 8),
    ("Roles", 1),
    ("RPC", 2),
    ("RSS", 3),
    ("RSS/Atom Reader", 1),
    ("Ruby", 1),
    ("Rust", 1),
    ("Saas", 2),
    ("Safe", 5),
    ("Sample Code", 2),
    ("Scene", 1),
    ("Schedule", 1),
    ("Scheduling", 3),
    ("Schema", 4),
    ("Science", 15),
    ("Scientific", 1),
    ("Scientific Simulation", 2),
    ("Scotty", 2),
    ("Screencast", 1),
    ("Screensaver", 1),
    ("SCRIPT", 1),
    ("Scripting", 10),
    ("SDR", 1),
    ("Search", 14),
    ("Security", 43),
    ("Selective", 1),
    ("Selenium", 2),
    ("Semantic Web", 3),
    ("Semigroupoids", 1),
    ("Semigroups", 1),
    ("Serialization", 31),
    ("Servant", 64),
    ("Serverless", 1),
    ("Service", 3),
    ("Services", 17),
    ("Set Theory", 1),
    ("Setup", 2),
    ("Shake", 13),
    ("Shell", 5),
    ("Si5351", 1),
    ("Signatures", 1),
    ("Silk", 1),
    ("Silly Tool", 1),
    ("SIMD", 3),
    ("Simple", 7),
    ("Simulation", 19),
    ("Singletons", 5),
    ("SMT", 23),
    ("Snap", 35),
    ("Snaplet-fay", 1),
    ("Socket", 1),
    ("Software", 4),
    ("Software Defined Radio", 2),
    ("Sorting", 4),
    ("Sound", 216),
    ("Source Code Analysis", 3),
    ("Source Tools", 1),
    ("Source-tools", 8),
    ("Spam", 5),
    ("Spellchecker", 1),
    ("SpreadSheet", 3),
    ("SQL", 1),
    ("SQLite", 1),
    ("Staged", 1),
    ("State Machines", 2),
    ("Static", 1),
    ("Static Analysis", 8),
    ("StaticAnalysis", 1),
    ("Statistical Modeling", 2),
    ("Statistics", 66),
    ("Steganography", 1),
    ("Stemming", 1),
    ("STM", 2),
    ("STM32", 4),
    ("Stochastic Control", 1),
    ("Stomp", 4),
    ("Stratux", 5),
    ("Stream", 1),
    ("Streaming", 61),
    ("Streamly", 17),
    ("String", 11),
    ("Structures", 2),
    ("Subscriptions", 1),
    ("Subversion", 1),
    ("Succinct Data Structures", 8),
    ("Support Vector Machine", 1),
    ("SVD", 1),
    ("Svg", 4),
    ("Swagger", 11),
    ("Symbolic Computation", 16),
    ("Syntax", 7),
    ("SyntComp", 1),
    ("Syslog", 1),
    ("SYstem", 785),
    ("System Tools", 2),
    ("Systems", 1),
    ("Tar", 5),
    ("Taskwarrior", 1),
    ("Tasty", 1),
    ("Tasty-kat", 1),
    ("Tax", 1),
    ("Teaching", 9),
    ("Telemetry", 3),
    ("Template", 9),
    ("Template Haskell", 46),
    ("Template-haskell", 1),
    ("TemplateHaskell", 3),
    ("Templating", 2),
    ("Tensors", 13),
    ("Terminal", 13),
    ("Terraform", 1),
    ("Test", 44),
    ("Testing", 435),
    ("Testing-hackage", 1),
    ("Text", 1029),
    ("Text Editor", 1),
    ("Text Recognition", 1),
    ("Text To Speech", 1),
    ("Text.PrettyPrint", 1),
    ("TH", 2),
    ("Theorem Provers", 52),
    ("Theorem Proving", 1),
    ("These", 7),
    ("This", 1),
    ("Time", 65),
    ("Time Uniform", 1),
    ("Time-frequency Distributions", 1),
    ("Timeout", 2),
    ("Tinytools", 1),
    ("TODO", 4),
    ("Tokenisation", 1),
    ("TOML", 6),
    ("Tonatona", 8),
    ("Tool", 2),
    ("Tooling", 5),
    ("Tools", 53),
    ("Topology", 1),
    ("TouchDesigner", 1),
    ("Trace", 13),
    ("Tracing", 3),
    ("Training", 1),
    ("Trans", 1),
    ("Transformation", 3),
    ("Transformers", 1),
    ("Translation", 1),
    ("Transpiler", 1),
    ("Tree", 5),
    ("Tree-sitter", 14),
    ("Tropical Geometry", 1),
    ("Truth Maintenance", 1),
    ("TUI", 4),
    ("TUI Tool", 1),
    ("Tutorials", 1),
    ("Type Errors", 1),
    ("Type Inference", 1),
    ("Type System", 42),
    ("Type Theory", 1),
    ("Type-safe", 1),
    ("Typechecking", 1),
    ("Typeclass", 1),
    ("TypeID", 1),
    ("Types", 13),
    ("TypeScript", 2),
    ("Typesystems", 1),
    ("Typography", 6),
    ("UI", 10),
    ("Unicode", 9),
    ("Unification", 4),
    ("Uniform", 8),
    ("Uniform Handling Of Some Pandoc Stuff", 1),
    ("Unikernel", 1),
    ("Unity3D", 1),
    ("Unknown", 3),
    ("Unlift", 1),
    ("Unsafe", 2),
    ("Ur/Web", 2),
    ("Urbit", 2),
    ("URI", 2),
    ("URL", 1),
    ("User Interface", 5),
    ("User Interfaces", 92),
    ("User-interface", 9),
    ("UserInterface", 4),
    ("Util", 6),
    ("Utilities", 15),
    ("Utility", 66),
    ("Utils", 113),
    ("UUID", 1),
    ("Uzbl", 1),
    ("Validation", 6),
    ("Validity", 12),
    ("Value", 6),
    ("Vector", 9),
    ("Verification", 1),
    ("Video", 4),
    ("Vinyl", 4),
    ("Visual Programming", 2),
    ("Visualization", 3),
    ("Vulkan", 2),
    ("Wai", 4),
    ("Warning", 1),
    ("Watch", 1),
    ("Water", 1),
    ("Web", 1990),
    ("Web Scraping", 1),
    ("Web Server", 1),
    ("Web XML", 1),
    ("Web Yesod", 1),
    ("Web-scraping", 1),
    ("WebAssembly", 2),
    ("WebDriver", 2),
    ("Webframework", 1),
    ("Welcome", 1),
    ("Wiki", 1),
    ("Workflow", 2),
    ("Wsjtx", 1),
    ("X11", 2),
    ("XDG", 1),
    ("XFCE", 1),
    ("Xlsx", 1),
    ("XML", 116),
    ("XMonad", 6),
    ("YAML", 3),
    ("Yampa", 1),
    ("Yesod", 104),
    ("Yi", 18),
    ("Zeromq", 1),
    ("Zift", 7),
    ("Zip", 2),
    ("Zippers", 2),
    ("ZLib", 1),
    ("Unclassified", 487)
  ]
