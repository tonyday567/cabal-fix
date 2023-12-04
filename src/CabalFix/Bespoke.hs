{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CabalFix.Bespoke where

import Data.ByteString
import Distribution.Fields
import CabalFix
import Data.ByteString.Char8 qualified as C
import Data.Map.Strict qualified as Map
import Data.String.Interpolate

yearList :: [(String, Int)]
yearList = [("numhask", 2016), ("mealy", 2013), ("box", 2017), ("formatn", 2016), ("prettychart", 2023), ("code", 2023), ("poker-fold", 2020), ("numhask-space", 2016), ("iqfeed", 2014), ("box-socket", 2017), ("numhask-array", 2016), ("euler", 2023), ("tonyday567", 2020), ("foo", 2023), ("web-rep", 2015), ("dotparse", 2022), ("perf", 2018), ("anal", 2023), ("research-hackage", 2022), ("chart-svg", 2017), ("ephemeral", 2020)]

replaceCopyright' :: ByteString -> [Field [ByteString]] -> [Field [ByteString]]
replaceCopyright' libdep fs = case mv of
  Nothing -> fs
  Just v -> addField AddReplace (Field (Name [] "copyright") [FieldLine [] v]) fs
  where
    mv = C.pack . ("Tony Day (c) " <>) . show <$> Map.lookup (C.unpack libdep) (Map.fromList yearList)

-- bool id (replaceCategory' libdep) (replaceCategory rcfg) $
replaceCategory' :: ByteString -> [Field [ByteString]] -> [Field [ByteString]]
replaceCategory' libdep fs = case mv of
  Nothing -> fs
  Just v -> addField AddReplace (Field (Name [] "category") [FieldLine [] v]) fs
  where
    mv = Map.lookup (C.unpack libdep) (Map.fromList catList)

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
