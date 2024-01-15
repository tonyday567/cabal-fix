{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Archive investigation
module CabalFix.Archive where

import CabalFix
import CabalFix.FlatParse (depP, runParser_, untilP)
import Codec.Archive.Tar qualified as Tar
import Control.Category ((>>>))
import Data.Bifunctor
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Either
import Data.Foldable
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Distribution.Parsec
import Distribution.Version
import FlatParse.Basic qualified as FP
import GHC.Generics
import System.Directory
import Optics.Extra
import qualified Algebra.Graph.ToGraph as ToGraph
import Data.Set qualified as Set
import Algebra.Graph
import DotParse qualified as Dot

-- | the cabal index
cabalIndex :: IO FilePath
cabalIndex = do
  h <- getHomeDirectory
  pure $ h <> "/.cabal/packages/hackage.haskell.org/01-index.tar"

-- | all the tar entries that represent packages of some kind.
cabalEntries :: IO [Tar.Entry]
cabalEntries = entryList . Tar.read <$> (BSL.readFile =<< cabalIndex)
  where
    entryList es = Tar.foldEntries (:) [] (error . show) es

-- | The naming convention in 01-index.tar
data FileName = FileName {nameFN :: ByteString, versionFN :: ByteString, filenameFN :: ByteString} deriving (Generic, Eq, Ord, Show)

-- | Convert a ByteString to a FileName. Errors on failure.
filename :: ByteString -> FileName
filename = runParser_ filenameP

-- | FileName parser
filenameP :: FP.Parser e FileName
filenameP = FileName <$> untilP '/' <*> untilP '/' <*> FP.takeRest

-- | cabal files
--
-- Discards stale versions with later revisions
cabals :: IO [(FileName, ByteString)]
cabals = do
  es <- cabalEntries
  let cs = first (runParser_ filenameP . FP.strToUtf8) <$> filter ((/= "package.json") . filenameFN . runParser_ filenameP . FP.strToUtf8 . fst) (filter (not . List.isSuffixOf "preferred-versions" . fst) $ [(fp, BSL.toStrict bs) | (fp, Tar.NormalFile bs _) <- (\e -> (Tar.entryPath e, Tar.entryContent e)) <$> es])
  pure $ Map.toList $ Map.fromList cs

-- | Assumes cabal entries are in chronological order and that the last version encountered is the
-- latest valid one.
latestCabals :: IO (Map.Map ByteString (Version, ByteString))
latestCabals = do
  cs <- CabalFix.Archive.cabals
  pure $ Map.fromListWith (\new old -> bool old new (fst new >= fst old)) $ (\(fn, bs) -> (nameFN fn, (getVersion fn, bs))) <$> cs
  where
    getVersion = fromMaybe undefined . simpleParsecBS . versionFN

-- | Latest successfully parsing 'CabalFields'
latestCabalFields:: IO (Map.Map ByteString (Version, CabalFields))
latestCabalFields = do
  lcs <- latestCabals
  let lcs' = second (parseCabalFields (defaultConfig & set #freeTexts [])) <$> lcs
  pure (second (fromRight undefined) <$> Map.filter (snd >>> isRight) lcs')



-- | extract library build-deps from a Field list, also looking in common stanzas
libDeps :: CabalFields -> [Dep]
libDeps cf = deps
  where
    libFields = cf & foldOf (#fields % fieldList' % section' "library" % each % secFields')
    libBds = libFields & foldOf (fieldValues' "build-depends")
    libDeps = runParser_ (FP.many depP) libBds
    libImports = libFields & toListOf (fieldValues' "import")
    cs = cf & foldOf (#fields % fieldList' % section' "common")
    libCommons = cs & filter (all (`elem` libImports) . toListOf (secArgs' % each % secArgBS' % _2))
    commonsBds = libCommons & foldOf (fieldValues' "build-depends")
    commonsDeps = runParser_ (FP.many depP) commonsBds
    deps = fmap (uncurry Dep) (libDeps <> commonsDeps)

-- | Map of valid dependencies
validLibDeps :: Map.Map ByteString CabalFields -> Map.Map ByteString [ByteString]
validLibDeps cs = ldeps
  where
    vlls = cs & Map.filter (view (#fields % fieldList' % section' "library") >>> length >>> (>0))
    ldeps' = vlls & fmap (libDeps >>> fmap dep >>> List.nub)
    bdnames = List.nub $ mconcat (snd <$> Map.toList ldeps')
    -- dependencies that do not exist in the main library list
    bdnames0 = filter (not . (`elem` Map.keys ldeps')) bdnames
    -- Exclude any library that has dependencies outside the universe.
    ldeps = ldeps' & Map.filter (any (`List.elem` bdnames0) >>> not)

-- | Graph of all valid dependencies
allDepGraph :: Map.Map ByteString CabalFields -> Graph ByteString
allDepGraph cs = transpose $ stars (Map.toList (validLibDeps cs))

-- | count distinct elements of a list.
count_ :: (Ord a) => [a] -> Map.Map a Int
count_ = foldl' (\x a -> Map.insertWith (+) a 1 x) Map.empty

-- | collect distinct monoidal values
collect_ :: (Ord k) => [(k, v)] -> Map.Map k [v]
collect_ = foldl' (\x (k, v) -> Map.insertWith (<>) k [v] x) Map.empty

-- | Get the set of upstream projects
upstreams :: ByteString -> Graph ByteString -> Set.Set ByteString
upstreams x g = Set.delete "base" $ ToGraph.preSet x g

-- | Get the set of downstream projects.
downstreams :: ByteString -> Graph ByteString -> Set.Set ByteString
downstreams x g = ToGraph.postSet x g

-- | Get the upstream graph of a library. text, for example:
--
upstreamG :: ByteString -> Graph ByteString -> Graph ByteString
upstreamG lib g = induce (`elem` toList supers) g
  where
    supers = upstreams lib g <> Set.singleton "text"

-- | Create a dot graph from an algebraic graph of dependencies
dotUpstream :: Graph ByteString -> ByteString
dotUpstream g = Dot.dotPrint Dot.defaultDotConfig g'
  where
    baseGraph = Dot.defaultGraph & Dot.attL Dot.GraphType (Dot.ID "size") .~ Just (Dot.IDQuoted "5!") & Dot.attL Dot.NodeType (Dot.ID "shape") .~ Just (Dot.ID "box") & Dot.attL Dot.NodeType (Dot.ID "height") .~ Just (Dot.ID "2") & Dot.gattL (Dot.ID "rankdir") .~ Just (Dot.IDQuoted "TB")
    g' = Dot.toDotGraphWith Dot.Directed baseGraph g

-- | make an svg file of a dependency graph
--
-- ![text example](other/textdeps.svg)
dotUpstreamSvg :: Graph ByteString -> FilePath -> IO ByteString
dotUpstreamSvg g svg = Dot.processDotWith Dot.Directed ["-Tsvg", "-o"<>svg] (dotUpstream g)
