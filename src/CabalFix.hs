{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module CabalFix
  ( Config (..),
    defaultConfig,
    cabalFix,
    cabalFixFile,
    getCabalFiles,
    filterChangedEdit,
    getCabalFile,
    sec,
    AddPolicy (..),
    CommaStyle (..),
    DepAlignment (..),
    ValueAlignment (..),
    Margin (..),
)
where

import CabalFix.FlatParse
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.Foldable
import Data.Function
import Data.Functor.Identity
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.TreeDiff hiding (FieldName)
import Data.TreeDiff qualified as T
import Data.TreeDiff.OMap qualified as O
import Distribution.Fields
import Distribution.Fields.Field hiding (fieldUniverse)
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Generic
import Distribution.Version
import GHC.Generics
import System.Directory
import Text.PrettyPrint qualified as PP
import Prelude

-- | Configuration values for varius aspects of rendering a cabal file.
data Config = Config
  { -- | fields that should be converted to free text
    freeTexts :: [ByteString],
    -- | fields that should be removed
    removals :: [ByteString],
    -- | Preferred dependency ranges
    preferredDeps :: [(ByteString, ByteString)],
    overwrites :: [(ByteString, ByteString, AddPolicy)],
    -- | Fields where CommaStyle should be checked and fixed.
    fixCommas :: [(ByteString, CommaStyle)],
    -- | Fields where elements should be sorted alphabetically
    sortFieldLines :: [ByteString],
    -- | Whether to sort Fields.
    sortFields :: Bool,
    -- | The preferred ordering of Fields if they are sorted (lower numbers are placed first).
    fieldOrdering :: [(ByteString, Double)],
    -- | Whether to fix the build dependency Field
    fixBuildDeps :: Bool,
    -- | How to align build dependencies
    depAlignment :: DepAlignment,
    -- | Whether to remove Fields with no information
    removeBlankFields :: Bool,
    -- | Whether to column-align values
    valueAligned :: ValueAlignment,
    -- | Margin between sections
    sectionMargin :: Margin,
    -- | Margin around comments
    commentMargin :: Margin,
    -- | Adopt narrow style bloew this column number.
    narrowN :: Int,
    -- | Indentation value
    indentN :: Int
  }
  deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig =
  Config
    ["description"]
    []
    defaultPreferredDeps
    []
    defaultFixCommas
    defaultFieldLineSorts
    True
    defaultFieldOrdering
    True
    DepAligned
    True
    ValueUnaligned
    Margin
    NoMargin
    60
    4

-- | The style for comma-separated values
data CommaStyle
  = -- | commas before values, including the first
    PrefixCommas
  | -- | commas after values, including the last
    PostfixCommas
  | -- | comma freedom
    FreeformCommas
  | -- | remove commas (allowed for some fields)
    NoCommas
  deriving (Eq, Show, Read)

-- | Policy for Fields listed in 'overwrites'
data AddPolicy
  = -- | Replace existing values
    AddReplace
  | -- | Append after existing values
    AddAppend
  | -- | Add only of the Field doesn't exist
    AddIfNotExisting
  deriving (Eq, Show, Read)

defaultFixCommas :: [(ByteString, CommaStyle)]
defaultFixCommas =
    [ ("extra-doc-files", NoCommas),
      ("build-depends", PrefixCommas)
    ]

-- | An opionated ordering of fields.
defaultFieldOrdering :: [(ByteString, Double)]
defaultFieldOrdering = [("cabal-version", 0), ("import", 1), ("main-is", 2), ("default-language", 3), ("name", 4), ("hs-source-dirs", 5), ("version", 6), ("build-depends", 7), ("exposed-modules", 8), ("license", 9), ("license-file", 10), ("other-modules", 11), ("copyright", 12), ("category", 13), ("author", 14), ("default-extensions", 15), ("ghc-options", 16), ("maintainer", 17), ("homepage", 18), ("bug-reports", 19), ("synopsis", 20), ("description", 21), ("build-type", 22), ("tested-with", 23), ("extra-doc-files", 24), ("source-repository", 25), ("type", 26), ("common", 27), ("location", 28), ("library", 29), ("executable", 30), ("test-suite", 31)]

-- | An opinionated list of fields whose elements should be sorted.
defaultFieldLineSorts :: [ByteString]
defaultFieldLineSorts =
  [ "build-depends",
    "exposed-modules",
    "default-extensions",
    "ghc-options",
    "extra-doc-files",
    "tested-with"
  ]

-- | An opinionated list of preferred builddeps:
--
-- >>> preferredDepsBS
defaultPreferredDeps :: [(ByteString, ByteString)]
defaultPreferredDeps = [("base", ">=4.7 && <5")]

cabalFix :: Config -> ByteString -> ByteString
cabalFix rcfg bs = (C.pack . showFields'' rcfg fComment (const id) (indentN rcfg) . fmap (fmap (fmap C.unpack)) . printFieldsComments $ fs') <> C.unlines extras
  where
    (fs, extras) = parseFieldsAndComments (freeTexts rcfg) bs
    -- position info is now gone
    (Field (Name _ _) (FieldLine _ libdep : _)) = head $ filter (hasName ["name"]) fs
    fs' =
      defaultSortFields rcfg $
        fmap (sortFieldLinesFor (sortFieldLines rcfg)) $
          bool id (fmap (fixBuildDeps' rcfg libdep)) (fixBuildDeps rcfg) $
            adds rcfg $
              fmap (fixcommas' rcfg) $
                bool id (filter (not . isBlankField)) (removeBlankFields rcfg) $
                  filter (not . hasName (removals rcfg)) fs
    fComment [] = NoComment
    fComment xs = CommentBefore xs

adds :: Config -> [Field [a]] -> [Field [a]]
adds rcfg x = foldl' (&) x $ overwrites rcfg & fmap (\(n, v, p) -> addField p (Field (Name [] n) [FieldLine [] v]))

fixcommas' :: Config -> Field ann -> Field ann
fixcommas' rcfg x = foldl' (&) x $ fixCommas rcfg & fmap (\(n, s) -> commas (hasName [n]) s)

fixBuildDeps' :: Config -> ByteString -> Field ann -> Field ann
fixBuildDeps' rcfg libdep (Field n@(Name _ "build-depends") fls) = Field n (fixBDLines libdep (depAlignment rcfg) fls)
fixBuildDeps' _ _ f@(Field _ _) = f
fixBuildDeps' rcfg libdep (Section n a fss) = Section n a (fixBuildDeps' rcfg libdep <$> fss)

fixBDLines :: ByteString -> DepAlignment -> [FieldLine ann] -> [FieldLine ann]
fixBDLines libdep align fls = fls'
  where
    deps = parseDepFL <$> fls
    pds = (", " <>) <$> printDepsPreferred libdep align deps
    fls' = zipWith setValueFL fls pds

parseDepFL :: FieldLine ann -> Dep
parseDepFL (FieldLine _ fl) = uncurry Dep $ runParser_ depP fl

setValueFL :: FieldLine ann -> ByteString -> FieldLine ann
setValueFL (FieldLine ann _) = FieldLine ann

getCabalFiles :: FilePath -> IO [(FilePath, ByteString)]
getCabalFiles dir =
  getDirectoryContents dir
    >>= ( \fs ->
            mapM (BS.readFile . (dir <>)) fs & fmap (zip fs)
        )
      . filter (not . List.isPrefixOf ".")

getCabalFile :: FilePath -> IO ByteString
getCabalFile = BS.readFile

cabalFixFile :: FilePath -> Config -> IO ()
cabalFixFile fp cfg = do
  bs <- getCabalFile fp
  BS.writeFile fp (cabalFix cfg bs)

toFields :: ByteString -> [Field Position]
toFields bs = either (error . show) id $ readFields bs

-- | Unification of field and section names
name :: Field a -> ByteString
name (Field (Name _ n) _) = n
name (Section (Name _ n) _ _) = n


-- | section deconstruction
sec :: FieldName -> Field ann -> Maybe ([SectionArg ann], [Field ann])
sec f (Section (Name _ n) sargs fs) = bool Nothing (Just (sargs, fs)) (f == n)
sec _ (Field _ _) = Nothing

-- | SectionArg name
secName :: SectionArg a -> (ByteString, ByteString)
secName (SecArgName _ n) = ("name", n)
secName (SecArgStr _ n) = ("str", n)
secName (SecArgOther _ n) = ("other", n)

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


-- | 'showFields' with user specified indentation.
showFields'' ::
  Config ->
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

renderFields :: Config -> Opts ann -> [PrettyField ann] -> [String]
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
  deriving (Show, Eq, Read)

data Margin = Margin | NoMargin
  deriving (Eq, Show, Read)

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

data ValueAlignment = ValueAligned | ValueUnaligned deriving (Eq, Show, Read)

lines_ :: String -> [String]
lines_ [] = []
lines_ s = lines s <> bool [] [""] ((== '\n') . last $ s)

renderField :: Config -> Opts ann -> Int -> PrettyField ann -> Block
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

addField :: AddPolicy -> Field ann -> [Field ann] -> [Field ann]
addField p f fs = case p of
  AddReplace -> notsames <> [f]
  AddAppend -> fs <> [f]
  AddIfNotExisting -> bool fs (fs <> [f]) (null sames)
  where
    sames = filter ((name f ==) . name) fs
    notsames = filter ((name f /=) . name) fs

data Dep = Dep {dep :: ByteString, depRange :: ByteString} deriving (Show, Ord, Eq, Generic)

normDepRange :: ByteString -> ByteString
normDepRange dr = (maybe dr (C.pack . show . pretty) . (simpleParsecBS :: ByteString -> Maybe VersionRange)) dr

data DepAlignment = DepAligned | DepUnaligned deriving (Eq, Show, Read)

printDepPreferred :: ByteString -> Int -> Dep -> ByteString
printDepPreferred libd n (Dep d r) = C.intercalate (C.pack $ replicate n ' ') ([d] <> rs)
  where
    -- FIXME: why is this defaultPreferredDeps
    r' = bool (normDepRange r) (fromMaybe (normDepRange r) (Map.lookup d (Map.fromList defaultPreferredDeps))) (libd == d)
    rs = bool [r'] [] (r' == "")

printDepsPreferred :: ByteString -> DepAlignment -> [Dep] -> [ByteString]
printDepsPreferred libd DepUnaligned ds = printDepPreferred libd 1 <$> ds
printDepsPreferred libd DepAligned ds = zipWith (printDepPreferred libd) ns ds
  where
    ls = BS.length . dep <$> ds
    ns = (\x -> maximum ls - x + 1) <$> ls

defaultSortFields :: Config -> [Field ann] -> [Field ann]
defaultSortFields cfg fs = List.sortOn (\f -> (fromMaybe 100 (Map.lookup (name f) (Map.fromList $ fieldOrdering cfg)), name2 f)) (sortInnerFields cfg <$> fs)

name2 :: Field ann -> Maybe ByteString
name2 (Field _ fl) = listToMaybe (fieldLineBS <$> fl)
name2 (Section _ a _) = listToMaybe $ snd . secName <$> a

sortInnerFields :: Config -> Field ann -> Field ann
sortInnerFields _ f@(Field _ _) = f
sortInnerFields cfg (Section n a fss) = Section n a (defaultSortFields cfg $ sortInnerFields cfg <$> fss)

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

