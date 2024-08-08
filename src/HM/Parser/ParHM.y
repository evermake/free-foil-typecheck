-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.5).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module HM.Parser.ParHM
  ( happyError
  , myLexer
  , pTypedExp
  , pExp2
  , pExp1
  , pExp
  , pType
  ) where

import Prelude

import qualified HM.Parser.AbsHM
import HM.Parser.LexHM

}

%name pTypedExp TypedExp
%name pExp2 Exp2
%name pExp1 Exp1
%name pExp Exp
%name pType Type
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('      { PT _ (TS _ 1)  }
  ')'      { PT _ (TS _ 2)  }
  '+'      { PT _ (TS _ 3)  }
  '-'      { PT _ (TS _ 4)  }
  '::'     { PT _ (TS _ 5)  }
  'Bool'   { PT _ (TS _ 6)  }
  'Nat'    { PT _ (TS _ 7)  }
  'else'   { PT _ (TS _ 8)  }
  'false'  { PT _ (TS _ 9)  }
  'if'     { PT _ (TS _ 10) }
  'iszero' { PT _ (TS _ 11) }
  'then'   { PT _ (TS _ 12) }
  'true'   { PT _ (TS _ 13) }
  L_integ  { PT _ (TI $$)   }

%%

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

TypedExp :: { HM.Parser.AbsHM.TypedExp }
TypedExp : Exp '::' Type { HM.Parser.AbsHM.TypedExp $1 $3 }

Exp2 :: { HM.Parser.AbsHM.Exp }
Exp2
  : 'true' { HM.Parser.AbsHM.ETrue }
  | 'false' { HM.Parser.AbsHM.EFalse }
  | Integer { HM.Parser.AbsHM.ENat $1 }
  | 'iszero' '(' Exp ')' { HM.Parser.AbsHM.EIsZero $3 }
  | '(' Exp ')' { $2 }

Exp1 :: { HM.Parser.AbsHM.Exp }
Exp1
  : Exp1 '+' Exp2 { HM.Parser.AbsHM.EAdd $1 $3 }
  | Exp1 '-' Exp2 { HM.Parser.AbsHM.ESub $1 $3 }
  | Exp2 { $1 }

Exp :: { HM.Parser.AbsHM.Exp }
Exp
  : 'if' Exp 'then' Exp 'else' Exp { HM.Parser.AbsHM.EIf $2 $4 $6 }
  | Exp1 { $1 }

Type :: { HM.Parser.AbsHM.Type }
Type
  : 'Nat' { HM.Parser.AbsHM.TNat } | 'Bool' { HM.Parser.AbsHM.TBool }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}

