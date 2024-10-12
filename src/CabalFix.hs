{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Tools to print, parse and fix cabal files.
module CabalFix
  ( -- * Usage
    -- $usage

    -- * Configuration
    Config (..),
    defaultConfig,
    AddPolicy (..),
    CommaStyle (..),
    CommaTrail (..),
    DepAlignment (..),
    ValueAlignment (..),
    Margin (..),

    -- * CabalFields
    Comment,
    CabalFields (..),
    cabalFields',
    fieldList',

    -- * Lenses
    -- $lenses
    topfield',
    field',
    subfield',
    section',
    secFields',
    fieldOrSection',
    overField,
    overFields,
    pname,
    fieldLines',
    fieldName',
    secArgs',
    secArgBS',
    fieldLine',
    fieldValues',

    -- * Parsing
    parseCabalFields,

    -- * Printing
    printCabalFields,

    -- * Fixes
    fixCabalFields,
    fixCabalFile,
    fixesCommas,
    addsFields,
    addField,
    fixBuildDeps,

    -- * Dependency
    Dep (..),

    -- * Examples
    minimalExampleBS,
    minimalConfig,
  )
where

import CabalFix.FlatParse (depP, runParserEither)
import Control.Category ((>>>))
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
import Data.String.Interpolate
import Data.TreeDiff hiding (FieldName)
import Data.TreeDiff.OMap qualified as OMap
import Data.Vector qualified as V
import Distribution.Fields
import Distribution.Fields.Field
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Generic
import Distribution.Version
import GHC.Generics hiding (to)
import Optics.Extra
import Text.PrettyPrint qualified as PP
import Prelude

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLabels
-- >>> :set -Wno-x-partial
-- >>> import CabalFix
-- >>> import Optics.Extra
-- >>> import Data.ByteString.Char8 qualified as C
-- >>> import CabalFix.Patch
-- >>> bs = minimalExampleBS
-- >>> cfg = defaultConfig
-- >>> (Just cf) = preview (cabalFields' cfg) bs
-- >>> fs = cf & view (#fields % fieldList')
-- >>> printCabalFields cfg (cf & over (#fields % fieldList') (take 4)) & C.putStr
-- cabal-version: 3.0
-- name: minimal
-- version: 0.1.0.0
-- license: BSD-2-Clause

-- $usage
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLabels
-- >>> import CabalFix
-- >>> import Optics.Extra
-- >>> import Data.ByteString.Char8 qualified as C
-- >>> import CabalFix.Patch
-- >>> bs = minimalExampleBS
-- >>> cfg = defaultConfig
-- >>> (Just cf) = preview (cabalFields' cfg) bs
-- >>> fs = cf & view (#fields % fieldList')
-- >>> printCabalFields cfg (cf & over (#fields % fieldList') (take 4)) & C.putStr
-- cabal-version: 3.0
-- name: minimal
-- version: 0.1.0.0
-- license: BSD-2-Clause

-- | Configuration values for various aspects of (re)rendering a cabal file.
data Config = Config
  { -- | fields that should be converted to free text
    freeTexts :: [ByteString],
    -- | fields that should be removed
    fieldRemovals :: [ByteString],
    -- | Preferred dependency ranges
    preferredDeps :: [(ByteString, ByteString)],
    -- | Add fields (Overwriting depends on an 'AddPolicy')
    addFields :: [(ByteString, ByteString, AddPolicy)],
    -- | Fields where CommaStyle should be checked and fixed.
    fixCommas :: [(ByteString, CommaStyle, CommaTrail)],
    -- | Fields where elements should be sorted alphabetically
    sortFieldLines :: [ByteString],
    -- | Whether to sort Fields.
    doSortFields :: Bool,
    -- | The preferred ordering of Fields if they are sorted (lower numbers are placed first).
    fieldOrdering :: [(ByteString, Double)],
    -- | Whether to fix the build dependency Field
    doFixBuildDeps :: Bool,
    -- | How to align build dependencies
    depAlignment :: DepAlignment,
    -- | Whether to remove Fields with no information
    removeBlankFields :: Bool,
    -- | Whether to column-align values
    valueAligned :: ValueAlignment,
    -- | The number of spaces between the field nameand the value, if aligned.
    valueAlignGap :: Int,
    -- | Margin between sections
    sectionMargin :: Margin,
    -- | Margin around comments
    commentMargin :: Margin,
    -- | Shift from narrow style to multi-line beyond this column size.
    narrowN :: Int,
    -- | Indentation value
    indentN :: Int
  }
  deriving (Eq, Show, Read, Generic)

-- | An opinionated configuration for formatting cabal files.
--
-- Some opinions (that can be configured):
--
-- >>> fixCommas defaultConfig
-- [("extra-doc-files",NoCommas,NoTrailer),("build-depends",PrefixCommas,Trailer)]
--
-- 'PrefixCommas' are better for the dependency list as dependency ranges are already noisy enough without a comma thrown in. 'Trailer' (which means leading comma for prefixed commas) is neater and easier to prepend to, append to & sort.
--
-- If a field list doesn't need commas, then they should be removed.
--
-- >>> preferredDeps defaultConfig
-- [("base",">=4.14 && <5")]
--
-- Standard practice compared with the much tighter eg @base ^>=4.17.2.1@
--
-- >>> sortFieldLines defaultConfig
-- ["build-depends","exposed-modules","default-extensions","ghc-options","extra-doc-files","tested-with"]
--
-- Sort all the things, but especially the module list.
--
-- >>> valueAligned defaultConfig
-- ValueUnaligned
--
-- Adding an extra, long-named field to the cabal file means we have to re-align all the value parts in all the other fields.
--
-- >>> depAlignment defaultConfig
-- DepAligned
--
-- build-depends is so busy, however, the extra alignment becomes more important.
--
-- >>> doSortFields defaultConfig
-- True
--
-- Whatever the order, fields should have the same order within each section.
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
    1
    Margin
    NoMargin
    60
    4

-- | The style for comma-separated values
data CommaStyle
  = -- | commas before values
    PrefixCommas
  | -- | commas after values
    PostfixCommas
  | -- | comma freedom
    FreeformCommas
  | -- | remove commas (allowed for some fields)
    NoCommas
  deriving (Eq, Show, Read, Generic)

-- | Include a trailing (or leading) comma, after the last value (or before the first value.)
data CommaTrail
  = Trailer
  | NoTrailer
  deriving (Eq, Show, Read, Generic)

-- | Policy for Fields listed in 'addFields'
data AddPolicy
  = -- | Replace existing values
    AddReplace
  | -- | Append after existing values
    AddAppend
  | -- | Add only of the Field doesn't exist
    AddIfNotExisting
  deriving (Eq, Show, Read, Generic)

defaultFixCommas :: [(ByteString, CommaStyle, CommaTrail)]
defaultFixCommas =
  [ ("extra-doc-files", NoCommas, NoTrailer),
    ("build-depends", PrefixCommas, Trailer)
  ]

-- | An opionated ordering of fields.
defaultFieldOrdering :: [(ByteString, Double)]
defaultFieldOrdering = [("cabal-version", 0), ("import", 1), ("main-is", 2), ("default-language", 3), ("name", 4), ("hs-source-dirs", 5), ("version", 6), ("build-depends", 7), ("exposed-modules", 8), ("license", 9), ("license-file", 10), ("other-modules", 11), ("copyright", 12), ("category", 13), ("author", 14), ("default-extensions", 15), ("ghc-options", 16), ("maintainer", 17), ("homepage", 18), ("bug-reports", 19), ("synopsis", 20), ("description", 21), ("build-type", 22), ("tested-with", 23), ("extra-doc-files", 24), ("source-repository", 25), ("type", 26), ("common", 27), ("location", 28), ("library", 29), ("executable", 30), ("test-suite", 31)]

-- An opinionated list of fields whose elements should be sorted.
defaultFieldLineSorts :: [ByteString]
defaultFieldLineSorts =
  [ "build-depends",
    "exposed-modules",
    "default-extensions",
    "ghc-options",
    "extra-doc-files",
    "tested-with"
  ]

-- An opinionated list of preferred builddeps:
--
defaultPreferredDeps :: [(ByteString, ByteString)]
defaultPreferredDeps = [("base", ">=4.14 && <5")]

-- | Whether the value part of each field should be vertically aligned on a column.
data ValueAlignment = ValueAligned | ValueUnaligned deriving (Eq, Show, Read, Generic)

-- | Whether the range part of the dependency list should be vertically aligned on a column.
data DepAlignment = DepAligned | DepUnaligned deriving (Eq, Show, Read)

-- | A margin tracker for combining sections.
data Margin = Margin | NoMargin
  deriving (Eq, Show, Read, Generic)

-- | Collapse margins, any margin = margin
instance Semigroup Margin where
  NoMargin <> NoMargin = NoMargin
  _ <> _ = Margin

-- | Note that cabal does not have multi-line comments
type Comment = [ByteString]

-- | 'Field' list annotated with a 'Comment'
--
-- Note that this type does not contain any position information.
--
-- The construction assumes that comments relate to fields below, so there is potential for an end comment unrelated to any particular field.
data CabalFields = CabalFields {fields :: V.Vector (Field Comment), endComment :: Comment} deriving (Generic, Eq, Show)

instance Semigroup CabalFields where
  (CabalFields fs ec) <> (CabalFields fs' ec') = CabalFields (fs <> fs') (ec <> ec')

instance Monoid CabalFields where
  mempty = CabalFields V.empty []

-- | iso to flip between vectors and lists easily.
--
-- >>> cf & view (#fields % fieldList') & take 2
-- [Field (Name [] "cabal-version") [FieldLine [] "3.0"],Field (Name [] "name") [FieldLine [] "minimal"]]
fieldList' :: Iso' (V.Vector (Field Comment)) [Field Comment]
fieldList' = iso V.toList V.fromList

instance ToExpr (FieldLine Comment) where
  toExpr fl = Rec "FieldLine" (OMap.fromList [("comment", toExpr (fieldLineAnn fl)), ("fieldline", toExpr (fieldLineBS fl))])

instance ToExpr (Name Comment) where
  toExpr n = Rec "Name" (OMap.fromList [("comment", toExpr (nameAnn n)), ("name", toExpr (getName n))])

instance ToExpr (SectionArg Comment) where
  toExpr (SecArgName c bs) = Rec "SecArgName" (OMap.fromList [("comment", toExpr c), ("arg", toExpr bs)])
  toExpr (SecArgStr c bs) = Rec "SecArgStr" (OMap.fromList [("comment", toExpr c), ("arg", toExpr bs)])
  toExpr (SecArgOther c bs) = Rec "SecArgOther" (OMap.fromList [("comment", toExpr c), ("arg", toExpr bs)])

instance ToExpr (Field Comment) where
  toExpr (Field n fls) = Rec "Field" (OMap.fromList [("name", toExpr n), ("field lines", toExpr fls)])
  toExpr (Section n ss fs) = Rec "Section" (OMap.fromList [("name", toExpr n), ("section args", toExpr ss), ("fields", toExpr fs)])

instance ToExpr CabalFields where
  toExpr cf = Rec "CabalFields" (OMap.fromList [("fields", toExpr $ fields cf), ("extras", toExpr $ endComment cf)])

-- | A Prism betwixt a 'ByteString' and a 'CabalFields'.
--
-- >>> cf & over (#fields % fieldList') (take 2) & review (cabalFields' cfg) & C.putStr
-- cabal-version: 3.0
-- name: minimal
cabalFields' :: Config -> Prism' ByteString CabalFields
cabalFields' cfg = prism (printCabalFields cfg) (parseCabalFields cfg)

-- $lenses
--
-- Lensing into 'Field' is tricky.
--
-- A 'Field' is a sum type of a field constructor or a section constructor, and a section contains fields.
--
-- Sometimes you only want to modify a field (and not a section). Other times you want to access a section but not a field. Sometimes you want to modify either a field or a section, and the fields within sections. It can be difficult to remember which lens is which.
--
-- The use of a list is also problematic; it is hard to safely delete a field, and invalid cabals are easily represented. A list can easily contain two name fields say, which is an invalid state. It can contain no name which is also invalid. It is difficult, however, to switch to a map because sections contain lists of fields (and not maps of fields).
--
-- Most useful are lenses that lens into named fields.

-- | A lens that doesn't descend into sections. It will lens the first-encountered named field, if any.
--
-- >>> view (topfield' "name") cf
-- Just (Field (Name [] "name") [FieldLine [] "minimal"])
--
-- >>> view (topfield' "build-depends") cf
-- Nothing
topfield' :: FieldName -> Lens' CabalFields (Maybe (Field Comment))
topfield' name = lens (view (#fields % fieldList' % field' name) >>> listToMaybe) (fieldSet name)

fieldSet :: FieldName -> CabalFields -> Maybe (Field Comment) -> CabalFields
fieldSet name cf f =
  case V.findIndex ((== name) . getName . fieldName) (view #fields cf) of
    Just i' -> case f of
      Nothing -> cf & over #fields (\v -> V.take i' v <> V.drop (i' + 1) v)
      Just f' -> cf & over #fields (\v -> V.take i' v <> V.singleton f' <> V.drop (i' + 1) v)
    Nothing -> cf & maybe id (over #fields . (\x -> (<> V.singleton x))) f

-- | A lens by name into a field (but not a section).
--
-- >>> fs & view (field' "version")
-- [Field (Name [] "version") [FieldLine [] "0.1.0.0"]]
field' :: FieldName -> Getter [Field Comment] [Field Comment]
field' name = to (filter (not . isSection) . filter (isName name))

-- | A getter by name into a field (including within sections)
--
-- >>> fs & toListOf (each % subfield' "default-language")
-- [[],[],[],[],[],[],[],[],[Field (Name [] "default-language") [FieldLine [] "GHC2021"]],[Field (Name [] "default-language") [FieldLine [] "GHC2021"]]]
subfield' :: FieldName -> Getter (Field Comment) [Field Comment]
subfield' name = to (subfield_ name)

subfield_ :: FieldName -> Field ann -> [Field ann]
subfield_ name f = filter (isName name) $ fieldUniverse f

-- | A getter of a section (not a field)
--
-- >>> fs & foldOf (section' "library" % each % secFields' % field' "exposed-modules")
-- [Field (Name [] "exposed-modules") [FieldLine [] "MyLib"]]
section' :: FieldName -> Getter [Field ann] [Field ann]
section' name = to (filter (\f -> isName name f && isSection f))

-- | A getter of section fields
secFields' :: Lens' (Field ann) [Field ann]
secFields' = lens secFieldsView secFieldsSet

secFieldsSet :: Field ann -> [Field ann] -> Field ann
secFieldsSet f@(Field {}) _ = f
secFieldsSet (Section n sa _) fs = Section n sa fs

secFieldsView :: Field ann -> [Field ann]
secFieldsView (Field {}) = []
secFieldsView (Section _ _ fs) = fs

-- | A getter by name of a field or section.
fieldOrSection' :: FieldName -> Getter [Field ann] [Field ann]
fieldOrSection' name = to (filter (isName name))

isName :: FieldName -> Field ann -> Bool
isName name = (== name) . view fieldName'

isSection :: Field ann -> Bool
isSection (Section {}) = True
isSection (Field {}) = False

-- | A mapping into the field structure, operating on field lists in sections as well as the field itself.
overField :: (Field ann -> Field ann) -> Field ann -> Field ann
overField f' f@(Field {}) = f' f
overField f' (Section n sa fs) = Section n sa (fmap (overField f') fs)

-- | A mapping into the field structure, operating on field lists in sections as well as field lists themselves.
overFields :: ([Field ann] -> [Field ann]) -> [Field ann] -> [Field ann]
overFields f fs = f $ fmap inner fs
  where
    inner f'@(Field {}) = f'
    inner (Section n sa fs') = Section n sa (overFields f fs')

-- | Project name. Errors if the field is missing.
--
-- >>> pname cf
-- "minimal"
pname :: CabalFields -> ByteString
pname cf = cf & preview (topfield' "name" % _Just % fieldLines' % ix 0 % to fieldLineBS) & fromMaybe (error "no name field")

-- | Name of (field or section).
--
-- >>> head fs & view fieldName'
-- "cabal-version"
fieldName' :: Lens' (Field ann) ByteString
fieldName' = lens (fieldName >>> getName) fieldNameSet
  where
    fieldNameSet (Field (Name ann _) fls) name = Field (Name ann name) fls
    fieldNameSet (Section (Name ann _) sa fs) name = Section (Name ann name) sa fs

inNameList :: [ByteString] -> Field ann -> Bool
inNameList ns f = view fieldName' f `elem` ns

-- | Lens into field lines
--
-- >>> fs & foldOf (section' "test-suite" % each % secFields' % field' "build-depends" % each % fieldLines')
-- [FieldLine [] "base ^>=4.17.2.1,",FieldLine [] "minimal"]
fieldLines' :: Lens' (Field ann) [FieldLine ann]
fieldLines' = lens fieldFieldLinesView fieldFieldLinesSet

fieldFieldLinesView :: Field ann -> [FieldLine ann]
fieldFieldLinesView (Field _ fls) = fls
fieldFieldLinesView _ = []

fieldFieldLinesSet :: Field ann -> [FieldLine ann] -> Field ann
fieldFieldLinesSet (Field n _) fls = Field n fls
fieldFieldLinesSet _ _ = error "setting a section field line"

-- * SectionArg

-- | lens into SectionArg part of a section.
--
-- Errors if you actually have a field.
--
-- >>> fs & foldOf (section' "test-suite" % each % secArgs')
-- [SecArgName [] "minimal-test"]
secArgs' :: Lens' (Field ann) [SectionArg ann]
secArgs' = lens secArgView secArgSet

secArgView :: Field ann -> [SectionArg ann]
secArgView (Field {}) = error "not a section"
secArgView (Section _ a _) = a

secArgSet :: Field ann -> [SectionArg ann] -> Field ann
secArgSet (Field {}) _ = error "not a section"
secArgSet (Section n _ fs) a = Section n a fs

-- | secArg lens into a ByteString representation
--
-- >>> fs & foldOf (section' "test-suite" % each % secArgs' % each % secArgBS')
-- ("name","minimal-test")
secArgBS' :: Lens' (SectionArg ann) (ByteString, ByteString)
secArgBS' = lens secArgBSView secArgBSSet

secArgBSView :: SectionArg a -> (ByteString, ByteString)
secArgBSView (SecArgName _ n) = ("name", n)
secArgBSView (SecArgStr _ n) = ("str", n)
secArgBSView (SecArgOther _ n) = ("other", n)

secArgBSSet :: SectionArg ann -> (ByteString, ByteString) -> SectionArg ann
secArgBSSet sa (t, a) = case t of
  "name" -> SecArgName (sectionArgAnn sa) a
  "str" -> SecArgStr (sectionArgAnn sa) a
  _ -> SecArgOther (sectionArgAnn sa) a

-- | lens into field line contents.
--
-- >>>  fs & toListOf (section' "test-suite" % each % secFields' % field' "build-depends" % each % fieldLines' % each % fieldLine')
-- ["base ^>=4.17.2.1,","minimal"]
fieldLine' :: Lens' (FieldLine ann) ByteString
fieldLine' = lens fieldLineBS setValueFL
  where
    setValueFL (FieldLine ann _) = FieldLine ann

-- | A fold of a field list into a ByteString.
fieldValues' :: FieldName -> Optic A_Fold '[Int, Int] [Field Comment] [Field Comment] ByteString ByteString
fieldValues' name = field' name % each % fieldLines' % each % fieldLine'

-- * fixes

-- | fix order:
--
-- - removes fields
--
-- - removes blank fields
--
-- - fixes commas
--
-- - adds Fields
--
-- - fix build dependencies
--
-- - sort field lines
--
-- - sort fields
fixCabalFields :: Config -> CabalFields -> CabalFields
fixCabalFields cfg cf =
  cf
    & over
      (#fields % fieldList')
      ( overFields (filter (not . inNameList (fieldRemovals cfg)))
          >>> overFields (bool id (filter (not . isBlankField)) (removeBlankFields cfg))
          >>> fmap (overField (fixesCommas cfg))
          >>> addsFields cfg
          >>> bool id (fmap (overField (fixBuildDeps cfg (pname cf)))) (doFixBuildDeps cfg)
          >>> fmap (overField (sortFieldLinesFor (sortFieldLines cfg)))
          >>> bool id (overFields (sortFields cfg)) (doSortFields cfg)
      )

-- | Fix a cabal file in-place
fixCabalFile :: FilePath -> Config -> IO Bool
fixCabalFile fp cfg = do
  bs <- BS.readFile fp
  maybe
    (pure False)
    (\cf -> BS.writeFile fp (printCabalFields cfg (fixCabalFields cfg cf)) >> pure True)
    (preview (cabalFields' cfg) bs)

-- * blank field removal

-- | Is the field blank (including has no section arguments if a section)
isBlankField :: Field ann -> Bool
isBlankField (Field _ fs) = null fs
isBlankField (Section _ sas fss) = null fss && null sas

-- * commas

-- | Fix the comma usage in a field list
--
-- >>> fs & toListOf (section' "test-suite" % each % secFields' % field' "build-depends" % each) & fmap (fixesCommas cfg)
-- [Field (Name [] "build-depends") [FieldLine [] ", base ^>=4.17.2.1",FieldLine [] ", minimal"]]
fixesCommas :: Config -> Field ann -> Field ann
fixesCommas cfg x = foldl' (&) x $ fixCommas cfg & fmap (\(n, s, t) -> bool id (fixCommasF s t) ((== n) $ view fieldName' x))

addCommaBS :: CommaStyle -> CommaTrail -> [ByteString] -> [ByteString]
addCommaBS commaStyle trailStyle xs = case trailStyle of
  NoTrailer -> case commaStyle of
    PostfixCommas -> ((<> ",") <$> init xs) <> [last xs]
    PrefixCommas -> head xs : ((", " <>) <$> tail xs)
    -- since we don't know the prior comma strategy, we just guess here.
    FreeformCommas -> ((<> ",") <$> init xs) <> [last xs]
    NoCommas -> xs
  Trailer -> case commaStyle of
    PostfixCommas -> (<> ",") <$> xs
    PrefixCommas -> (", " <>) <$> xs
    -- since we don't know the prior comma strategy, we just guess here.
    FreeformCommas -> (<> ",") <$> xs
    NoCommas -> xs

stripCommaBS :: ByteString -> ByteString
stripCommaBS bs =
  C.stripPrefix ", " bs
    & fromMaybe
      ( C.stripPrefix "," bs
          & fromMaybe
            ( C.stripSuffix "," bs
                & fromMaybe bs
            )
      )

fixCommasF :: CommaStyle -> CommaTrail -> Field ann -> Field ann
fixCommasF s t f = fls'
  where
    fls = toListOf (fieldLines' % each % fieldLine') f
    fls' = set fieldLines' (zipWith (set fieldLine') (addCommaBS s t $ fmap stripCommaBS fls) (view fieldLines' f)) f

-- | add fields
--
-- >>> addsFields (cfg & set #addFields [("description", "added by addsFields", AddReplace)]) []
-- [Field (Name [] "description") [FieldLine [] "added by addsFields"]]
addsFields :: Config -> [Field Comment] -> [Field Comment]
addsFields cfg x = foldl' (&) x $ addFields cfg & fmap (\(n, v, p) -> addField p (Field (Name [] n) [FieldLine [] v]))

-- | Add a field according to an AddPolicy.
addField :: AddPolicy -> Field ann -> [Field ann] -> [Field ann]
addField p f fs = case p of
  AddReplace -> notsames <> [f]
  AddAppend -> fs <> [f]
  AddIfNotExisting -> bool fs (fs <> [f]) (null sames)
  where
    sames = filter ((view fieldName' f ==) . view fieldName') fs
    notsames = filter ((view fieldName' f /=) . view fieldName') fs

-- | Align dependencies (if depAlignment is DepAligned), remove ranges for any self-dependency, and substitute preferred dependency ranges.
--
-- >>> fs & toListOf (section' "test-suite" % each % secFields' % field' "build-depends" % each) & fmap (fixBuildDeps cfg "minimal")
-- [Field (Name [] "build-depends") [FieldLine [] ", base    >=4.14 && <5",FieldLine [] ", minimal"]]
fixBuildDeps :: Config -> FieldName -> Field ann -> Field ann
fixBuildDeps cfg pn f = overField (bool id (over fieldLines' (fixBDLines cfg pn)) (isName "build-depends" f)) f

fixBDLines :: Config -> ByteString -> [FieldLine ann] -> [FieldLine ann]
fixBDLines cfg libdep fls = fls'
  where
    align = depAlignment cfg
    deps = [x | (Right x) <- parseDepFL <$> fls]
    pds = addCommaBS commaStyle trailStyle $ printDepsPreferred cfg libdep align deps
    fls' = zipWith (set fieldLine') pds fls

    (commaStyle, trailStyle) =
      maybe
        (PostfixCommas, NoTrailer)
        (\(_, x, y) -> (x, y))
        (find ((== "build-depends") . (\(x, _, _) -> x)) (fixCommas cfg))

-- | Split of a dependency 'FieldLine' into the dependency name and the range.
data Dep = Dep {dep :: ByteString, depRange :: ByteString} deriving (Show, Ord, Eq, Generic)

normDepRange :: ByteString -> ByteString
normDepRange dr = (maybe dr (C.pack . show . pretty) . (simpleParsecBS :: ByteString -> Maybe VersionRange)) dr

printDepPreferred :: Config -> ByteString -> Int -> Dep -> ByteString
printDepPreferred cfg libd n (Dep d r) = C.intercalate (C.pack $ replicate n ' ') ([d] <> rs)
  where
    r' = bool (fromMaybe (normDepRange r) (Map.lookup d (Map.fromList (preferredDeps cfg)))) (normDepRange r) (libd == d)
    rs = bool [r'] [] (r' == "")

printDepsPreferred :: Config -> ByteString -> DepAlignment -> [Dep] -> [ByteString]
printDepsPreferred cfg libd DepUnaligned ds = printDepPreferred cfg libd 1 <$> ds
printDepsPreferred cfg libd DepAligned ds = zipWith (printDepPreferred cfg libd) ns ds
  where
    ls = BS.length . dep <$> ds
    ns = (\x -> maximum ls - x + 1) <$> ls

parseDepFL :: FieldLine ann -> Either ByteString Dep
parseDepFL fl = uncurry Dep <$> runParserEither depP (view fieldLine' fl)

-- | sort field lines for listed fields
sortFieldLinesFor :: [ByteString] -> Field ann -> Field ann
sortFieldLinesFor ns f@(Field n fl) =
  Field n (bool fl (List.sortOn fieldLineBS fl) (view fieldName' f `elem` ns))
sortFieldLinesFor ns (Section n a fss) = Section n a (sortFieldLinesFor ns <$> fss)

-- | sorting fields, based on fieldOrdering configuration.
--
-- A secondary ordering is based on the first fieldline (for fields) or section args (for sections).
sortFields :: Config -> [Field ann] -> [Field ann]
sortFields cfg fs = overFields (List.sortOn (\f -> (fromMaybe 100 (Map.lookup (view fieldName' f) (Map.fromList $ fieldOrdering cfg)), name2 f))) fs

name2 :: Field ann -> Maybe ByteString
name2 (Field _ fl) = listToMaybe (fieldLineBS <$> fl)
name2 (Section _ a _) = listToMaybe $ snd . view secArgBS' <$> a

-- | Printing
--
-- Convert a 'CabalFields' to a 'ByteString'
--
-- >>> printCabalFields cfg (cf & over (#fields % fieldList') (take 4)) & C.putStr
-- cabal-version: 3.0
-- name: minimal
-- version: 0.1.0.0
-- license: BSD-2-Clause
printCabalFields :: Config -> CabalFields -> ByteString
printCabalFields cfg cf =
  ( C.pack
      . showFieldsIndent cfg fComment (const id) (indentN cfg)
      . fmap (fmap (fmap C.unpack))
      . printFieldsComments
      $ view (#fields % fieldList') cf
  )
    <> C.unlines (view #endComment cf)
  where
    fComment [] = NoComment
    fComment xs = CommentBefore xs

printFieldsComments :: [Field [ByteString]] -> [PrettyField [ByteString]]
printFieldsComments =
  runIdentity
    . genericFromParsecFields
      (Identity .: prettyFieldLines)
      (Identity .: prettySectionArgs)
  where
    (.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
    (f .: g) x y = f (g x y)

-- | Used in 'fromParsecFields'.
prettyFieldLines :: FieldName -> [FieldLine [ByteString]] -> PP.Doc
prettyFieldLines _ fls =
  PP.vcat $
    mconcat $
      [ PP.text . fromUTF8BS <$> cs <> [bs]
      | FieldLine cs bs <- fls
      ]

-- | Used in 'fromParsecFields'.
prettySectionArgs :: FieldName -> [SectionArg [ByteString]] -> [PP.Doc]
prettySectionArgs _ =
  fmap $
    mconcat . \case
      SecArgName cs bs -> showToken . fromUTF8BS <$> cs <> [bs]
      SecArgStr cs bs -> showToken . fromUTF8BS <$> cs <> [bs]
      SecArgOther cs bs -> PP.text . fromUTF8BS <$> cs <> [bs]

-- | 'showFields' with user specified indentation.
showFieldsIndent ::
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
showFieldsIndent cfg rann post n = unlines . renderFields cfg (Opts rann indent post)
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
renderFields cfg opts fields = flattenBlocks blocks
  where
    len = maxNameLength 0 fields
    blocks =
      filter (not . null . _contentsBlock) $ -- empty blocks cause extra newlines #8236
        map (renderField cfg opts len) fields

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
  deriving (Show, Eq, Read, Generic)

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

lines_ :: String -> [String]
lines_ [] = []
lines_ s = lines s <> bool [] [""] ((== '\n') . last $ s)

renderField :: Config -> Opts ann -> Int -> PrettyField ann -> Block
renderField cfg (Opts rann indent post) fw (PrettyField ann name doc) =
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
      _ -> commentMargin cfg

    (lines', after) = case lines_ narrow of
      [] -> ([name' ++ ":"], NoMargin)
      [singleLine]
        | length singleLine < narrowN cfg ->
            ([name' ++ ": " ++ replicate (bool 0 (fw - length name' + (valueAlignGap cfg - 1)) (valueAligned cfg == ValueAligned)) ' ' ++ narrow], NoMargin)
      _ -> ((name' ++ ":") : map indent (lines_ (PP.render doc)), NoMargin)

    name' = fromUTF8BS name
    narrow = PP.renderStyle narrowStyle doc

    narrowStyle :: PP.Style
    narrowStyle = PP.style {PP.lineLength = PP.lineLength PP.style - fw}
renderField cfg opts@(Opts rann indent post) _ (PrettySection ann name args fields) =
  Block (sectionMargin cfg) (sectionMargin cfg) $
    attachComments
      (post ann [PP.render $ PP.hsep $ PP.text (fromUTF8BS name) : args])
      ++ map indent (renderFields cfg opts fields)
  where
    attachComments content = case rann ann of
      CommentBefore cs -> cs ++ content
      CommentAfter cs -> content ++ cs
      NoComment -> content
renderField _ _ _ PrettyEmpty = Block NoMargin NoMargin mempty

-- | Parse a 'ByteString' into a 'CabalFields'. Failure is possible.
--
-- >>> bs & C.lines & take 4 & C.unlines & parseCabalFields cfg
-- Right (CabalFields {fields = [Field (Name [] "cabal-version") [FieldLine [] "3.0"],Field (Name [] "name") [FieldLine [] "minimal"],Field (Name [] "version") [FieldLine [] "0.1.0.0"],Field (Name [] "license") [FieldLine [] "BSD-2-Clause"]], endComment = []})
parseCabalFields :: Config -> ByteString -> Either ByteString CabalFields
parseCabalFields cfg bs = case readFields bs of
  Left err -> Left $ C.pack (show err)
  Right fps ->
    (\(fs', ec) -> Right (CabalFields (V.fromList fs') ec)) $
      foldl' (&) (fmap (fmap (const [])) fs, []) (uncurry addComment <$> cfs)
    where
      fs = convertFreeTexts (view #freeTexts cfg) fps
      cs = extractComments bs
      pt = Map.toList $ makePositionTree fs
      cfs = fmap (first (fmap snd)) (first (fmap (pt List.!!) . (\x -> List.findIndex (\e -> fst e > x) pt)) <$> cs)

convertFreeText :: [ByteString] -> Field Position -> Field Position
convertFreeText freeTexts f@(Field n fls) = bool f (Field n (convertToFreeText fls)) (inNameList freeTexts f)
convertFreeText freeTexts (Section n a fss) = Section n a (convertFreeTexts freeTexts fss)

convertFreeTexts :: [ByteString] -> [Field Position] -> [Field Position]
convertFreeTexts freeTexts fs = snd $ foldl' step (Nothing, []) fs
  where
    step :: (Maybe (Field Position), [Field Position]) -> Field Position -> (Maybe (Field Position), [Field Position])
    step (Nothing, res) nextFP = case inNameList freeTexts nextFP of
      True -> (Just (convertFreeText freeTexts nextFP), res)
      False -> (Nothing, res <> [nextFP])
    step (Just freeFP, res) nextFP = case inNameList freeTexts nextFP of
      True -> (Just (convertFreeText freeTexts nextFP), res <> [freeFP'])
      False -> (Nothing, res <> [freeFP', nextFP])
      where
        (Field n fls) = freeFP
        c1 = firstCol nextFP
        c0 = fromMaybe c1 $ firstColFls freeFP
        (FieldLine ann fls') = fromMaybe (FieldLine (Position 0 0) "") (listToMaybe fls)
        freeFP' = Field n [FieldLine ann (fls' <> C.pack (replicate (c1 - c0 - length (C.lines fls')) '\n'))]

firstCol :: Field Position -> Int
firstCol (Field (Name (Position c _) _) _) = c
firstCol (Section (Name (Position c _) _) _ _) = c

firstColFls :: Field Position -> Maybe Int
firstColFls (Field _ []) = Nothing
firstColFls (Field _ ((FieldLine (Position c _) _) : _)) = Just c
firstColFls (Section {}) = error "no field lines in a section"

convertToFreeText :: [FieldLine Position] -> [FieldLine Position]
convertToFreeText [] = []
convertToFreeText ((FieldLine (Position r0 c0) bs0) : xs) = [FieldLine (Position r0 c0) x]
  where
    x = mconcat $ snd $ foldl' (\(r', xs') (FieldLine (Position r _) bs) -> (r, xs' <> replicate (r - r') "\n" <> [bs])) (r0, [bs0]) xs

extractComments :: BS.ByteString -> [(Int, Comment)]
extractComments = go . zip [1 ..] . map (BS.dropWhile isSpace8) . C.lines
  where
    go :: [(Int, BS.ByteString)] -> [(Int, Comment)]
    go [] = []
    go ((n, bs) : rest)
      | isComment bs = case span ((isComment .|| BS.null) . snd) rest of
          (h, t) -> (n, bs : map snd h) : go t
      | otherwise = go rest

    (f .|| g) x = f x || g x

    isSpace8 w = w == 9 || w == 32

    isComment :: BS.ByteString -> Bool
    isComment = BS.isPrefixOf "--"

data FieldPath
  = End
  | Nth Int FieldPath -- nth field
  deriving (Eq, Ord, Show)

makePositionTree :: [Field Position] -> Map.Map Int ([Int], String)
makePositionTree fs = foldFss Map.empty [] fs
  where
    foldFss m cursor fs' = fst $ foldl' stepFss (m, cursor <> [0]) fs'
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
addc _ [] _ fs = fs
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

-- | Minimal cabal file contents for testing purposes. Originally created via:
--
-- > mkdir minimal && cd minimal && cabal init --minimal --simple --overwrite --lib --tests --language=GHC2021 --license=BSD-2-Clause  -p minimal
minimalExampleBS :: ByteString
minimalExampleBS =
  [i|cabal-version:   3.0
name:            minimal
version:         0.1.0.0
license:         BSD-2-Clause
license-file:    LICENSE
build-type:      Simple
extra-doc-files: CHANGELOG.md

common warnings
    ghc-options: -Wall

library
    import:           warnings
    exposed-modules:  MyLib
    build-depends:    base ^>=4.17.2.1
    hs-source-dirs:   src
    default-language: GHC2021

test-suite minimal-test
    import:           warnings
    default-language: GHC2021
    type:             exitcode-stdio-1.0
    hs-source-dirs:   test
    main-is:          Main.hs
    build-depends:
        base ^>=4.17.2.1,
        minimal|]

-- | A config close to the @cabal init@ styles.
minimalConfig :: Config
minimalConfig =
  Config
    { freeTexts = ["description"],
      fieldRemovals = [],
      preferredDeps =
        [ ( "base",
            ">=4.14 && <5"
          )
        ],
      addFields = [],
      fixCommas =
        [ ( "extra-doc-files",
            NoCommas,
            NoTrailer
          ),
          ( "build-depends",
            PostfixCommas,
            NoTrailer
          )
        ],
      sortFieldLines =
        [ "build-depends",
          "exposed-modules",
          "default-extensions",
          "ghc-options",
          "extra-doc-files",
          "tested-with"
        ],
      doSortFields = True,
      fieldOrdering =
        [ ( "cabal-version",
            0.0
          ),
          ( "import",
            1.0
          ),
          ( "main-is",
            2.0
          ),
          ( "default-language",
            8.6
          ),
          ( "name",
            4.0
          ),
          ( "hs-source-dirs",
            8.4
          ),
          ( "version",
            6.0
          ),
          ( "build-depends",
            8.2
          ),
          ( "exposed-modules",
            8.0
          ),
          ( "license",
            9.0
          ),
          ( "license-file",
            10.0
          ),
          ( "other-modules",
            11.0
          ),
          ( "copyright",
            12.0
          ),
          ( "category",
            13.0
          ),
          ( "author",
            14.0
          ),
          ( "default-extensions",
            15.0
          ),
          ( "ghc-options",
            16.0
          ),
          ( "maintainer",
            17.0
          ),
          ( "homepage",
            18.0
          ),
          ( "bug-reports",
            19.0
          ),
          ( "synopsis",
            20.0
          ),
          ( "description",
            21.0
          ),
          ( "build-type",
            22.0
          ),
          ( "tested-with",
            23.0
          ),
          ( "extra-doc-files",
            24.0
          ),
          ( "source-repository",
            25.0
          ),
          ( "type",
            26.0
          ),
          ( "common",
            27.0
          ),
          ( "location",
            28.0
          ),
          ( "library",
            29.0
          ),
          ( "executable",
            30.0
          ),
          ( "test-suite",
            31.0
          )
        ],
      doFixBuildDeps = True,
      depAlignment = DepAligned,
      removeBlankFields = True,
      valueAligned = ValueAligned,
      valueAlignGap = 1,
      sectionMargin = Margin,
      commentMargin = Margin,
      narrowN = 60,
      indentN = 4
    }
