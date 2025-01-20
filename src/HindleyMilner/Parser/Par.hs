{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module HindleyMilner.Parser.Par
  ( happyError
  , myLexer
  , pPattern
  , pExp3
  , pExp2
  , pExp1
  , pExp
  , pScopedExp
  , pType2
  , pType1
  , pType
  , pScopedType
  , pTypePattern
  ) where

import Prelude

import qualified HindleyMilner.Parser.Abs
import HindleyMilner.Parser.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn14 (HindleyMilner.Parser.Abs.Ident)
	| HappyAbsSyn15 (Integer)
	| HappyAbsSyn16 (HindleyMilner.Parser.Abs.UVarIdent)
	| HappyAbsSyn17 (HindleyMilner.Parser.Abs.Pattern)
	| HappyAbsSyn18 (HindleyMilner.Parser.Abs.Exp)
	| HappyAbsSyn22 (HindleyMilner.Parser.Abs.ScopedExp)
	| HappyAbsSyn23 (HindleyMilner.Parser.Abs.Type)
	| HappyAbsSyn26 (HindleyMilner.Parser.Abs.ScopedType)
	| HappyAbsSyn27 (HindleyMilner.Parser.Abs.TypePattern)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,313) ([0,0,0,16,0,8,13316,0,2048,33792,52,0,8,15788,0,2048,44032,61,0,8,15788,0,2048,48,80,0,12296,20480,0,2048,4144,80,0,12296,20480,0,0,0,16,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,2048,4144,80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24576,0,0,0,0,0,0,0,0,0,0,8,15788,0,0,0,0,0,0,4096,0,2048,44032,61,0,8,0,0,0,0,16,0,0,0,0,0,0,16,0,0,0,0,2048,33796,52,0,0,0,0,2048,33792,52,0,96,0,0,0,0,0,0,0,0,0,0,0,0,0,8,13316,0,2048,1024,52,0,96,0,0,2048,4144,80,0,256,0,0,0,8,0,0,8,15788,0,2048,33792,54,0,0,64,0,4096,0,0,0,256,0,0,4096,0,0,0,12296,20480,0,0,0,0,0,0,0,0,2048,48,80,0,0,0,0,0,64,0,0,8,15788,0,4096,0,0,0,8,15788,0,2048,44032,61,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,13508,0,0,0,0,0,8,13446,0,2048,44032,61,0,0,0,0,2048,33794,52,0,8,15788,0,2048,44032,61,0,0,0,0,0,0,0,0,8,15788,0,2048,33920,52,0,0,1,0,2048,44032,61,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pPattern","%start_pExp3","%start_pExp2","%start_pExp1","%start_pExp","%start_pScopedExp","%start_pType2","%start_pType1","%start_pType","%start_pScopedType","%start_pTypePattern","Ident","Integer","UVarIdent","Pattern","Exp3","Exp2","Exp1","Exp","ScopedExp","Type2","Type1","Type","ScopedType","TypePattern","'('","')'","'+'","'-'","'->'","'.'","'..'","':'","'='","'Bool'","'Nat'","'['","']'","'do'","'else'","'false'","'for'","'forall'","'if'","'in'","'iszero'","'let'","'then'","'true'","'\955'","L_Ident","L_integ","L_UVarIdent","%eof"]
        bit_start = st Prelude.* 56
        bit_end = (st Prelude.+ 1) Prelude.* 56
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..55]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (53) = happyShift action_12
action_0 (14) = happyGoto action_49
action_0 (17) = happyGoto action_50
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (28) = happyShift action_35
action_1 (43) = happyShift action_36
action_1 (51) = happyShift action_41
action_1 (53) = happyShift action_12
action_1 (54) = happyShift action_43
action_1 (14) = happyGoto action_29
action_1 (15) = happyGoto action_30
action_1 (18) = happyGoto action_48
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (28) = happyShift action_35
action_2 (43) = happyShift action_36
action_2 (48) = happyShift action_39
action_2 (51) = happyShift action_41
action_2 (53) = happyShift action_12
action_2 (54) = happyShift action_43
action_2 (14) = happyGoto action_29
action_2 (15) = happyGoto action_30
action_2 (18) = happyGoto action_31
action_2 (19) = happyGoto action_47
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (28) = happyShift action_35
action_3 (43) = happyShift action_36
action_3 (44) = happyShift action_37
action_3 (46) = happyShift action_38
action_3 (48) = happyShift action_39
action_3 (49) = happyShift action_40
action_3 (51) = happyShift action_41
action_3 (52) = happyShift action_42
action_3 (53) = happyShift action_12
action_3 (54) = happyShift action_43
action_3 (14) = happyGoto action_29
action_3 (15) = happyGoto action_30
action_3 (18) = happyGoto action_31
action_3 (19) = happyGoto action_32
action_3 (20) = happyGoto action_46
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (28) = happyShift action_35
action_4 (43) = happyShift action_36
action_4 (44) = happyShift action_37
action_4 (46) = happyShift action_38
action_4 (48) = happyShift action_39
action_4 (49) = happyShift action_40
action_4 (51) = happyShift action_41
action_4 (52) = happyShift action_42
action_4 (53) = happyShift action_12
action_4 (54) = happyShift action_43
action_4 (14) = happyGoto action_29
action_4 (15) = happyGoto action_30
action_4 (18) = happyGoto action_31
action_4 (19) = happyGoto action_32
action_4 (20) = happyGoto action_44
action_4 (21) = happyGoto action_45
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (28) = happyShift action_35
action_5 (43) = happyShift action_36
action_5 (44) = happyShift action_37
action_5 (46) = happyShift action_38
action_5 (48) = happyShift action_39
action_5 (49) = happyShift action_40
action_5 (51) = happyShift action_41
action_5 (52) = happyShift action_42
action_5 (53) = happyShift action_12
action_5 (54) = happyShift action_43
action_5 (14) = happyGoto action_29
action_5 (15) = happyGoto action_30
action_5 (18) = happyGoto action_31
action_5 (19) = happyGoto action_32
action_5 (20) = happyGoto action_33
action_5 (22) = happyGoto action_34
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (28) = happyShift action_20
action_6 (37) = happyShift action_21
action_6 (38) = happyShift action_22
action_6 (53) = happyShift action_12
action_6 (55) = happyShift action_23
action_6 (14) = happyGoto action_15
action_6 (16) = happyGoto action_16
action_6 (23) = happyGoto action_28
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (28) = happyShift action_20
action_7 (37) = happyShift action_21
action_7 (38) = happyShift action_22
action_7 (53) = happyShift action_12
action_7 (55) = happyShift action_23
action_7 (14) = happyGoto action_15
action_7 (16) = happyGoto action_16
action_7 (23) = happyGoto action_17
action_7 (24) = happyGoto action_27
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (28) = happyShift action_20
action_8 (37) = happyShift action_21
action_8 (38) = happyShift action_22
action_8 (45) = happyShift action_26
action_8 (53) = happyShift action_12
action_8 (55) = happyShift action_23
action_8 (14) = happyGoto action_15
action_8 (16) = happyGoto action_16
action_8 (23) = happyGoto action_17
action_8 (24) = happyGoto action_24
action_8 (25) = happyGoto action_25
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (28) = happyShift action_20
action_9 (37) = happyShift action_21
action_9 (38) = happyShift action_22
action_9 (53) = happyShift action_12
action_9 (55) = happyShift action_23
action_9 (14) = happyGoto action_15
action_9 (16) = happyGoto action_16
action_9 (23) = happyGoto action_17
action_9 (24) = happyGoto action_18
action_9 (26) = happyGoto action_19
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (53) = happyShift action_12
action_10 (14) = happyGoto action_13
action_10 (27) = happyGoto action_14
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (53) = happyShift action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_11

action_13 _ = happyReduce_43

action_14 (56) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_36

action_16 _ = happyReduce_33

action_17 (32) = happyShift action_63
action_17 _ = happyReduce_39

action_18 _ = happyReduce_42

action_19 (56) = happyAccept
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (28) = happyShift action_20
action_20 (37) = happyShift action_21
action_20 (38) = happyShift action_22
action_20 (45) = happyShift action_26
action_20 (53) = happyShift action_12
action_20 (55) = happyShift action_23
action_20 (14) = happyGoto action_15
action_20 (16) = happyGoto action_16
action_20 (23) = happyGoto action_17
action_20 (24) = happyGoto action_24
action_20 (25) = happyGoto action_62
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_35

action_22 _ = happyReduce_34

action_23 _ = happyReduce_13

action_24 _ = happyReduce_41

action_25 (56) = happyAccept
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (53) = happyShift action_12
action_26 (14) = happyGoto action_13
action_26 (27) = happyGoto action_61
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (56) = happyAccept
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (56) = happyAccept
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_15

action_30 _ = happyReduce_18

action_31 _ = happyReduce_23

action_32 (30) = happyShift action_51
action_32 (31) = happyShift action_52
action_32 _ = happyReduce_29

action_33 (28) = happyShift action_35
action_33 (43) = happyShift action_36
action_33 (48) = happyShift action_39
action_33 (51) = happyShift action_41
action_33 (53) = happyShift action_12
action_33 (54) = happyShift action_43
action_33 (14) = happyGoto action_29
action_33 (15) = happyGoto action_30
action_33 (18) = happyGoto action_31
action_33 (19) = happyGoto action_53
action_33 _ = happyReduce_32

action_34 (56) = happyAccept
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (28) = happyShift action_35
action_35 (43) = happyShift action_36
action_35 (44) = happyShift action_37
action_35 (46) = happyShift action_38
action_35 (48) = happyShift action_39
action_35 (49) = happyShift action_40
action_35 (51) = happyShift action_41
action_35 (52) = happyShift action_42
action_35 (53) = happyShift action_12
action_35 (54) = happyShift action_43
action_35 (14) = happyGoto action_29
action_35 (15) = happyGoto action_30
action_35 (18) = happyGoto action_31
action_35 (19) = happyGoto action_32
action_35 (20) = happyGoto action_44
action_35 (21) = happyGoto action_60
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_17

action_37 (53) = happyShift action_12
action_37 (14) = happyGoto action_49
action_37 (17) = happyGoto action_59
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (28) = happyShift action_35
action_38 (43) = happyShift action_36
action_38 (44) = happyShift action_37
action_38 (46) = happyShift action_38
action_38 (48) = happyShift action_39
action_38 (49) = happyShift action_40
action_38 (51) = happyShift action_41
action_38 (52) = happyShift action_42
action_38 (53) = happyShift action_12
action_38 (54) = happyShift action_43
action_38 (14) = happyGoto action_29
action_38 (15) = happyGoto action_30
action_38 (18) = happyGoto action_31
action_38 (19) = happyGoto action_32
action_38 (20) = happyGoto action_58
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (28) = happyShift action_57
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (53) = happyShift action_12
action_40 (14) = happyGoto action_49
action_40 (17) = happyGoto action_56
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_16

action_42 (53) = happyShift action_12
action_42 (14) = happyGoto action_49
action_42 (17) = happyGoto action_55
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_12

action_44 (28) = happyShift action_35
action_44 (35) = happyShift action_54
action_44 (43) = happyShift action_36
action_44 (48) = happyShift action_39
action_44 (51) = happyShift action_41
action_44 (53) = happyShift action_12
action_44 (54) = happyShift action_43
action_44 (14) = happyGoto action_29
action_44 (15) = happyGoto action_30
action_44 (18) = happyGoto action_31
action_44 (19) = happyGoto action_53
action_44 _ = happyReduce_31

action_45 (56) = happyAccept
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (28) = happyShift action_35
action_46 (43) = happyShift action_36
action_46 (48) = happyShift action_39
action_46 (51) = happyShift action_41
action_46 (53) = happyShift action_12
action_46 (54) = happyShift action_43
action_46 (56) = happyAccept
action_46 (14) = happyGoto action_29
action_46 (15) = happyGoto action_30
action_46 (18) = happyGoto action_31
action_46 (19) = happyGoto action_53
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (30) = happyShift action_51
action_47 (31) = happyShift action_52
action_47 (56) = happyAccept
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (56) = happyAccept
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_14

action_50 (56) = happyAccept
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (28) = happyShift action_35
action_51 (43) = happyShift action_36
action_51 (51) = happyShift action_41
action_51 (53) = happyShift action_12
action_51 (54) = happyShift action_43
action_51 (14) = happyGoto action_29
action_51 (15) = happyGoto action_30
action_51 (18) = happyGoto action_75
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (28) = happyShift action_35
action_52 (43) = happyShift action_36
action_52 (51) = happyShift action_41
action_52 (53) = happyShift action_12
action_52 (54) = happyShift action_43
action_52 (14) = happyGoto action_29
action_52 (15) = happyGoto action_30
action_52 (18) = happyGoto action_74
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (30) = happyShift action_51
action_53 (31) = happyShift action_52
action_53 _ = happyReduce_27

action_54 (28) = happyShift action_20
action_54 (37) = happyShift action_21
action_54 (38) = happyShift action_22
action_54 (45) = happyShift action_26
action_54 (53) = happyShift action_12
action_54 (55) = happyShift action_23
action_54 (14) = happyGoto action_15
action_54 (16) = happyGoto action_16
action_54 (23) = happyGoto action_17
action_54 (24) = happyGoto action_24
action_54 (25) = happyGoto action_73
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (33) = happyShift action_72
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (36) = happyShift action_71
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (28) = happyShift action_35
action_57 (43) = happyShift action_36
action_57 (44) = happyShift action_37
action_57 (46) = happyShift action_38
action_57 (48) = happyShift action_39
action_57 (49) = happyShift action_40
action_57 (51) = happyShift action_41
action_57 (52) = happyShift action_42
action_57 (53) = happyShift action_12
action_57 (54) = happyShift action_43
action_57 (14) = happyGoto action_29
action_57 (15) = happyGoto action_30
action_57 (18) = happyGoto action_31
action_57 (19) = happyGoto action_32
action_57 (20) = happyGoto action_44
action_57 (21) = happyGoto action_70
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (28) = happyShift action_35
action_58 (43) = happyShift action_36
action_58 (48) = happyShift action_39
action_58 (50) = happyShift action_69
action_58 (51) = happyShift action_41
action_58 (53) = happyShift action_12
action_58 (54) = happyShift action_43
action_58 (14) = happyGoto action_29
action_58 (15) = happyGoto action_30
action_58 (18) = happyGoto action_31
action_58 (19) = happyGoto action_53
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (47) = happyShift action_68
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (29) = happyShift action_67
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (33) = happyShift action_66
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (29) = happyShift action_65
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (28) = happyShift action_20
action_63 (37) = happyShift action_21
action_63 (38) = happyShift action_22
action_63 (53) = happyShift action_12
action_63 (55) = happyShift action_23
action_63 (14) = happyGoto action_15
action_63 (16) = happyGoto action_16
action_63 (23) = happyGoto action_17
action_63 (24) = happyGoto action_64
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_38

action_65 _ = happyReduce_37

action_66 (28) = happyShift action_20
action_66 (37) = happyShift action_21
action_66 (38) = happyShift action_22
action_66 (53) = happyShift action_12
action_66 (55) = happyShift action_23
action_66 (14) = happyGoto action_15
action_66 (16) = happyGoto action_16
action_66 (23) = happyGoto action_17
action_66 (24) = happyGoto action_18
action_66 (26) = happyGoto action_81
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_19

action_68 (39) = happyShift action_80
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (28) = happyShift action_35
action_69 (43) = happyShift action_36
action_69 (44) = happyShift action_37
action_69 (46) = happyShift action_38
action_69 (48) = happyShift action_39
action_69 (49) = happyShift action_40
action_69 (51) = happyShift action_41
action_69 (52) = happyShift action_42
action_69 (53) = happyShift action_12
action_69 (54) = happyShift action_43
action_69 (14) = happyGoto action_29
action_69 (15) = happyGoto action_30
action_69 (18) = happyGoto action_31
action_69 (19) = happyGoto action_32
action_69 (20) = happyGoto action_79
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (29) = happyShift action_78
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (28) = happyShift action_35
action_71 (43) = happyShift action_36
action_71 (44) = happyShift action_37
action_71 (46) = happyShift action_38
action_71 (48) = happyShift action_39
action_71 (49) = happyShift action_40
action_71 (51) = happyShift action_41
action_71 (52) = happyShift action_42
action_71 (53) = happyShift action_12
action_71 (54) = happyShift action_43
action_71 (14) = happyGoto action_29
action_71 (15) = happyGoto action_30
action_71 (18) = happyGoto action_31
action_71 (19) = happyGoto action_32
action_71 (20) = happyGoto action_77
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (28) = happyShift action_35
action_72 (43) = happyShift action_36
action_72 (44) = happyShift action_37
action_72 (46) = happyShift action_38
action_72 (48) = happyShift action_39
action_72 (49) = happyShift action_40
action_72 (51) = happyShift action_41
action_72 (52) = happyShift action_42
action_72 (53) = happyShift action_12
action_72 (54) = happyShift action_43
action_72 (14) = happyGoto action_29
action_72 (15) = happyGoto action_30
action_72 (18) = happyGoto action_31
action_72 (19) = happyGoto action_32
action_72 (20) = happyGoto action_33
action_72 (22) = happyGoto action_76
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_30

action_74 _ = happyReduce_21

action_75 _ = happyReduce_20

action_76 _ = happyReduce_26

action_77 (28) = happyShift action_35
action_77 (43) = happyShift action_36
action_77 (47) = happyShift action_84
action_77 (48) = happyShift action_39
action_77 (51) = happyShift action_41
action_77 (53) = happyShift action_12
action_77 (54) = happyShift action_43
action_77 (14) = happyGoto action_29
action_77 (15) = happyGoto action_30
action_77 (18) = happyGoto action_31
action_77 (19) = happyGoto action_53
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_22

action_79 (28) = happyShift action_35
action_79 (42) = happyShift action_83
action_79 (43) = happyShift action_36
action_79 (48) = happyShift action_39
action_79 (51) = happyShift action_41
action_79 (53) = happyShift action_12
action_79 (54) = happyShift action_43
action_79 (14) = happyGoto action_29
action_79 (15) = happyGoto action_30
action_79 (18) = happyGoto action_31
action_79 (19) = happyGoto action_53
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (28) = happyShift action_35
action_80 (43) = happyShift action_36
action_80 (44) = happyShift action_37
action_80 (46) = happyShift action_38
action_80 (48) = happyShift action_39
action_80 (49) = happyShift action_40
action_80 (51) = happyShift action_41
action_80 (52) = happyShift action_42
action_80 (53) = happyShift action_12
action_80 (54) = happyShift action_43
action_80 (14) = happyGoto action_29
action_80 (15) = happyGoto action_30
action_80 (18) = happyGoto action_31
action_80 (19) = happyGoto action_32
action_80 (20) = happyGoto action_82
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_40

action_82 (28) = happyShift action_35
action_82 (34) = happyShift action_87
action_82 (43) = happyShift action_36
action_82 (48) = happyShift action_39
action_82 (51) = happyShift action_41
action_82 (53) = happyShift action_12
action_82 (54) = happyShift action_43
action_82 (14) = happyGoto action_29
action_82 (15) = happyGoto action_30
action_82 (18) = happyGoto action_31
action_82 (19) = happyGoto action_53
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (28) = happyShift action_35
action_83 (43) = happyShift action_36
action_83 (44) = happyShift action_37
action_83 (46) = happyShift action_38
action_83 (48) = happyShift action_39
action_83 (49) = happyShift action_40
action_83 (51) = happyShift action_41
action_83 (52) = happyShift action_42
action_83 (53) = happyShift action_12
action_83 (54) = happyShift action_43
action_83 (14) = happyGoto action_29
action_83 (15) = happyGoto action_30
action_83 (18) = happyGoto action_31
action_83 (19) = happyGoto action_32
action_83 (20) = happyGoto action_86
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (28) = happyShift action_35
action_84 (43) = happyShift action_36
action_84 (44) = happyShift action_37
action_84 (46) = happyShift action_38
action_84 (48) = happyShift action_39
action_84 (49) = happyShift action_40
action_84 (51) = happyShift action_41
action_84 (52) = happyShift action_42
action_84 (53) = happyShift action_12
action_84 (54) = happyShift action_43
action_84 (14) = happyGoto action_29
action_84 (15) = happyGoto action_30
action_84 (18) = happyGoto action_31
action_84 (19) = happyGoto action_32
action_84 (20) = happyGoto action_33
action_84 (22) = happyGoto action_85
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_25

action_86 (28) = happyShift action_35
action_86 (43) = happyShift action_36
action_86 (48) = happyShift action_39
action_86 (51) = happyShift action_41
action_86 (53) = happyShift action_12
action_86 (54) = happyShift action_43
action_86 (14) = happyGoto action_29
action_86 (15) = happyGoto action_30
action_86 (18) = happyGoto action_31
action_86 (19) = happyGoto action_53
action_86 _ = happyReduce_24

action_87 (28) = happyShift action_35
action_87 (43) = happyShift action_36
action_87 (44) = happyShift action_37
action_87 (46) = happyShift action_38
action_87 (48) = happyShift action_39
action_87 (49) = happyShift action_40
action_87 (51) = happyShift action_41
action_87 (52) = happyShift action_42
action_87 (53) = happyShift action_12
action_87 (54) = happyShift action_43
action_87 (14) = happyGoto action_29
action_87 (15) = happyGoto action_30
action_87 (18) = happyGoto action_31
action_87 (19) = happyGoto action_32
action_87 (20) = happyGoto action_88
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (28) = happyShift action_35
action_88 (40) = happyShift action_89
action_88 (43) = happyShift action_36
action_88 (48) = happyShift action_39
action_88 (51) = happyShift action_41
action_88 (53) = happyShift action_12
action_88 (54) = happyShift action_43
action_88 (14) = happyGoto action_29
action_88 (15) = happyGoto action_30
action_88 (18) = happyGoto action_31
action_88 (19) = happyGoto action_53
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (41) = happyShift action_90
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (28) = happyShift action_35
action_90 (43) = happyShift action_36
action_90 (44) = happyShift action_37
action_90 (46) = happyShift action_38
action_90 (48) = happyShift action_39
action_90 (49) = happyShift action_40
action_90 (51) = happyShift action_41
action_90 (52) = happyShift action_42
action_90 (53) = happyShift action_12
action_90 (54) = happyShift action_43
action_90 (14) = happyGoto action_29
action_90 (15) = happyGoto action_30
action_90 (18) = happyGoto action_31
action_90 (19) = happyGoto action_32
action_90 (20) = happyGoto action_33
action_90 (22) = happyGoto action_91
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_28

happyReduce_11 = happySpecReduce_1  14 happyReduction_11
happyReduction_11 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn14
		 (HindleyMilner.Parser.Abs.Ident happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  15 happyReduction_12
happyReduction_12 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn15
		 ((read happy_var_1) :: Integer
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  16 happyReduction_13
happyReduction_13 (HappyTerminal (PT _ (T_UVarIdent happy_var_1)))
	 =  HappyAbsSyn16
		 (HindleyMilner.Parser.Abs.UVarIdent happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  17 happyReduction_14
happyReduction_14 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn17
		 (HindleyMilner.Parser.Abs.PatternVar happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  18 happyReduction_15
happyReduction_15 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.EVar happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  18 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.ETrue
	)

happyReduce_17 = happySpecReduce_1  18 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.EFalse
	)

happyReduce_18 = happySpecReduce_1  18 happyReduction_18
happyReduction_18 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.ENat happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  18 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  19 happyReduction_20
happyReduction_20 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.EAdd happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  19 happyReduction_21
happyReduction_21 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.ESub happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 19 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.EIsZero happy_var_3
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  19 happyReduction_23
happyReduction_23 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happyReduce 6 20 happyReduction_24
happyReduction_24 ((HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.EIf happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 6 20 happyReduction_25
happyReduction_25 ((HappyAbsSyn22  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.ELet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 4 20 happyReduction_26
happyReduction_26 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.EAbs happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_2  20 happyReduction_27
happyReduction_27 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.EApp happy_var_1 happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 10 20 happyReduction_28
happyReduction_28 ((HappyAbsSyn22  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.EFor happy_var_2 happy_var_5 happy_var_7 happy_var_10
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  20 happyReduction_29
happyReduction_29 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  21 happyReduction_30
happyReduction_30 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (HindleyMilner.Parser.Abs.ETyped happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  21 happyReduction_31
happyReduction_31 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  22 happyReduction_32
happyReduction_32 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn22
		 (HindleyMilner.Parser.Abs.ScopedExp happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  23 happyReduction_33
happyReduction_33 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn23
		 (HindleyMilner.Parser.Abs.TUVar happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  23 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn23
		 (HindleyMilner.Parser.Abs.TNat
	)

happyReduce_35 = happySpecReduce_1  23 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn23
		 (HindleyMilner.Parser.Abs.TBool
	)

happyReduce_36 = happySpecReduce_1  23 happyReduction_36
happyReduction_36 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn23
		 (HindleyMilner.Parser.Abs.TVar happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  23 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  24 happyReduction_38
happyReduction_38 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (HindleyMilner.Parser.Abs.TArrow happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  24 happyReduction_39
happyReduction_39 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happyReduce 4 25 happyReduction_40
happyReduction_40 ((HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (HindleyMilner.Parser.Abs.TForAll happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_1  25 happyReduction_41
happyReduction_41 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  26 happyReduction_42
happyReduction_42 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn26
		 (HindleyMilner.Parser.Abs.ScopedType happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  27 happyReduction_43
happyReduction_43 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn27
		 (HindleyMilner.Parser.Abs.TPatternVar happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 56 56 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 28;
	PT _ (TS _ 2) -> cont 29;
	PT _ (TS _ 3) -> cont 30;
	PT _ (TS _ 4) -> cont 31;
	PT _ (TS _ 5) -> cont 32;
	PT _ (TS _ 6) -> cont 33;
	PT _ (TS _ 7) -> cont 34;
	PT _ (TS _ 8) -> cont 35;
	PT _ (TS _ 9) -> cont 36;
	PT _ (TS _ 10) -> cont 37;
	PT _ (TS _ 11) -> cont 38;
	PT _ (TS _ 12) -> cont 39;
	PT _ (TS _ 13) -> cont 40;
	PT _ (TS _ 14) -> cont 41;
	PT _ (TS _ 15) -> cont 42;
	PT _ (TS _ 16) -> cont 43;
	PT _ (TS _ 17) -> cont 44;
	PT _ (TS _ 18) -> cont 45;
	PT _ (TS _ 19) -> cont 46;
	PT _ (TS _ 20) -> cont 47;
	PT _ (TS _ 21) -> cont 48;
	PT _ (TS _ 22) -> cont 49;
	PT _ (TS _ 23) -> cont 50;
	PT _ (TS _ 24) -> cont 51;
	PT _ (TS _ 25) -> cont 52;
	PT _ (TV happy_dollar_dollar) -> cont 53;
	PT _ (TI happy_dollar_dollar) -> cont 54;
	PT _ (T_UVarIdent happy_dollar_dollar) -> cont 55;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 56 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pPattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

pExp3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pScopedExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

pType2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

pType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

pScopedType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

pTypePattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
