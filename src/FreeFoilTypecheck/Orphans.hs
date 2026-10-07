{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'ZipMatchK' instances for the literals of the object languages.
--
-- Two nodes match only if their literals are equal, so α-equivalence and
-- unification distinguish nodes that differ only in a literal.
module FreeFoilTypecheck.Orphans () where

import Data.ZipMatchK (ZipMatchK (..), zipMatchViaEq)

instance ZipMatchK Integer where
  zipMatchWithK = zipMatchViaEq
