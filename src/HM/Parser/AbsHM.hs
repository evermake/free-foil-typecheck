-- File generated by the BNF Converter (bnfc 2.9.5).

-- | The abstract syntax of language HM.

module HM.Parser.AbsHM where

import Prelude (Integer)
import qualified Prelude as C (Eq, Ord, Show, Read)

data Exp = ETrue | EFalse | ENat Integer | EAdd Exp Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

