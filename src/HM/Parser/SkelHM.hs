-- File generated by the BNF Converter (bnfc 2.9.5).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module HM.Parser.SkelHM where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified HM.Parser.AbsHM

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transCommand :: HM.Parser.AbsHM.Command -> Result
transCommand x = case x of
  HM.Parser.AbsHM.CommandCheck typedexp -> failure x
  HM.Parser.AbsHM.CommandEval exp -> failure x

transTypedExp :: HM.Parser.AbsHM.TypedExp -> Result
transTypedExp x = case x of
  HM.Parser.AbsHM.TypedExp exp type_ -> failure x

transExp :: HM.Parser.AbsHM.Exp -> Result
transExp x = case x of
  HM.Parser.AbsHM.ETrue -> failure x
  HM.Parser.AbsHM.EFalse -> failure x
  HM.Parser.AbsHM.ENat integer -> failure x
  HM.Parser.AbsHM.EAdd exp1 exp2 -> failure x
  HM.Parser.AbsHM.ESub exp1 exp2 -> failure x
  HM.Parser.AbsHM.EIf exp1 exp2 exp3 -> failure x
  HM.Parser.AbsHM.EIsZero exp -> failure x

transType :: HM.Parser.AbsHM.Type -> Result
transType x = case x of
  HM.Parser.AbsHM.TNat -> failure x
  HM.Parser.AbsHM.TBool -> failure x
