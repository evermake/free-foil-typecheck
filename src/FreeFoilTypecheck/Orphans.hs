{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'ZipMatchK' instances for the literals of the object languages.
--
-- Matching two nodes ignores their literals and keeps the left one, so
-- α-equivalence and unification compare only the constructors and the
-- subterms of nodes.
module FreeFoilTypecheck.Orphans () where

import Data.ZipMatchK (ZipMatchK (..), zipMatchViaChooseLeft)

instance ZipMatchK Integer where
  zipMatchWithK = zipMatchViaChooseLeft
