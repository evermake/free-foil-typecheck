{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module FreeFoilTypecheck.HindleyMilner.Parser.Par
  ( happyError
  , myLexer
  , pPattern
  , pExp4
  , pExp3
  , pExp2
  , pExp1
  , pExp
  , pScopedExp
  , pTypePattern
  , pType2
  , pType1
  , pType
  , pScopedType
  ) where

import Prelude

import qualified FreeFoilTypecheck.HindleyMilner.Parser.Abs
import FreeFoilTypecheck.HindleyMilner.Parser.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn15 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.Ident)
	| HappyAbsSyn16 (Integer)
	| HappyAbsSyn17 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.UVarIdent)
	| HappyAbsSyn18 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.Pattern)
	| HappyAbsSyn19 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.Exp)
	| HappyAbsSyn24 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.ScopedExp)
	| HappyAbsSyn25 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.TypePattern)
	| HappyAbsSyn26 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.Type)
	| HappyAbsSyn29 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.ScopedType)

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
 action_91,
 action_92 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

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
 happyReduce_43,
 happyReduce_44,
 happyReduce_45 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,276) ([0,0,0,64,0,128,16448,3,0,2,3361,0,2048,33792,52,0,32,63152,0,32768,49152,986,0,512,27392,15,0,0,4096,0,8192,192,320,0,128,3,5,0,3074,5124,0,2048,48,80,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,2048,4144,80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1536,0,0,0,2,3361,0,0,0,0,0,0,0,0,32768,49152,986,0,0,0,0,0,0,4096,0,8192,45056,246,0,128,16448,3,0,0,1024,0,0,0,0,0,0,16384,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,128,18496,3,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,512,256,13,0,8,13316,0,32768,1,0,0,128,259,5,0,64,0,0,0,8,0,0,0,0,0,0,0,32,0,0,4096,0,0,16,0,0,0,4,0,0,256,0,0,0,3074,5120,0,0,0,0,0,0,0,0,32768,768,1280,0,0,0,0,0,16384,0,0,8192,45056,246,0,128,56000,3,0,2,3947,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,2048,0,0,128,56000,3,0,0,0,0,0,2,0,0,32,63152,0,32768,49152,986,0,0,0,0,0,0,0,0,8192,45056,246,0,0,8,0,0,16384,0,0,2048,44032,61,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pPattern","%start_pExp4","%start_pExp3","%start_pExp2","%start_pExp1","%start_pExp","%start_pScopedExp","%start_pTypePattern","%start_pType2","%start_pType1","%start_pType","%start_pScopedType","Ident","Integer","UVarIdent","Pattern","Exp4","Exp3","Exp2","Exp1","Exp","ScopedExp","TypePattern","Type2","Type1","Type","ScopedType","'('","')'","'+'","'-'","'->'","'.'","'..'","':'","'='","'Bool'","'Nat'","'['","']'","'do'","'else'","'false'","'for'","'forall'","'if'","'in'","'iszero'","'let'","'then'","'true'","'\955'","L_Ident","L_integ","L_UVarIdent","%eof"]
        bit_start = st Prelude.* 58
        bit_end = (st Prelude.+ 1) Prelude.* 58
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..57]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (55) = happyShift action_13
action_0 (15) = happyGoto action_52
action_0 (18) = happyGoto action_53
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (30) = happyShift action_37
action_1 (45) = happyShift action_38
action_1 (53) = happyShift action_43
action_1 (55) = happyShift action_13
action_1 (56) = happyShift action_45
action_1 (15) = happyGoto action_30
action_1 (16) = happyGoto action_31
action_1 (19) = happyGoto action_51
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (30) = happyShift action_37
action_2 (45) = happyShift action_38
action_2 (50) = happyShift action_41
action_2 (53) = happyShift action_43
action_2 (55) = happyShift action_13
action_2 (56) = happyShift action_45
action_2 (15) = happyGoto action_30
action_2 (16) = happyGoto action_31
action_2 (19) = happyGoto action_32
action_2 (20) = happyGoto action_50
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (30) = happyShift action_37
action_3 (45) = happyShift action_38
action_3 (50) = happyShift action_41
action_3 (53) = happyShift action_43
action_3 (55) = happyShift action_13
action_3 (56) = happyShift action_45
action_3 (15) = happyGoto action_30
action_3 (16) = happyGoto action_31
action_3 (19) = happyGoto action_32
action_3 (20) = happyGoto action_33
action_3 (21) = happyGoto action_49
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (30) = happyShift action_37
action_4 (45) = happyShift action_38
action_4 (46) = happyShift action_39
action_4 (48) = happyShift action_40
action_4 (50) = happyShift action_41
action_4 (51) = happyShift action_42
action_4 (53) = happyShift action_43
action_4 (54) = happyShift action_44
action_4 (55) = happyShift action_13
action_4 (56) = happyShift action_45
action_4 (15) = happyGoto action_30
action_4 (16) = happyGoto action_31
action_4 (19) = happyGoto action_32
action_4 (20) = happyGoto action_33
action_4 (21) = happyGoto action_34
action_4 (22) = happyGoto action_48
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (30) = happyShift action_37
action_5 (45) = happyShift action_38
action_5 (46) = happyShift action_39
action_5 (48) = happyShift action_40
action_5 (50) = happyShift action_41
action_5 (51) = happyShift action_42
action_5 (53) = happyShift action_43
action_5 (54) = happyShift action_44
action_5 (55) = happyShift action_13
action_5 (56) = happyShift action_45
action_5 (15) = happyGoto action_30
action_5 (16) = happyGoto action_31
action_5 (19) = happyGoto action_32
action_5 (20) = happyGoto action_33
action_5 (21) = happyGoto action_34
action_5 (22) = happyGoto action_46
action_5 (23) = happyGoto action_47
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (30) = happyShift action_37
action_6 (45) = happyShift action_38
action_6 (46) = happyShift action_39
action_6 (48) = happyShift action_40
action_6 (50) = happyShift action_41
action_6 (51) = happyShift action_42
action_6 (53) = happyShift action_43
action_6 (54) = happyShift action_44
action_6 (55) = happyShift action_13
action_6 (56) = happyShift action_45
action_6 (15) = happyGoto action_30
action_6 (16) = happyGoto action_31
action_6 (19) = happyGoto action_32
action_6 (20) = happyGoto action_33
action_6 (21) = happyGoto action_34
action_6 (22) = happyGoto action_35
action_6 (24) = happyGoto action_36
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (55) = happyShift action_13
action_7 (15) = happyGoto action_28
action_7 (25) = happyGoto action_29
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (30) = happyShift action_19
action_8 (39) = happyShift action_20
action_8 (40) = happyShift action_21
action_8 (55) = happyShift action_13
action_8 (57) = happyShift action_22
action_8 (15) = happyGoto action_14
action_8 (17) = happyGoto action_15
action_8 (26) = happyGoto action_27
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (30) = happyShift action_19
action_9 (39) = happyShift action_20
action_9 (40) = happyShift action_21
action_9 (55) = happyShift action_13
action_9 (57) = happyShift action_22
action_9 (15) = happyGoto action_14
action_9 (17) = happyGoto action_15
action_9 (26) = happyGoto action_16
action_9 (27) = happyGoto action_26
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (30) = happyShift action_19
action_10 (39) = happyShift action_20
action_10 (40) = happyShift action_21
action_10 (47) = happyShift action_25
action_10 (55) = happyShift action_13
action_10 (57) = happyShift action_22
action_10 (15) = happyGoto action_14
action_10 (17) = happyGoto action_15
action_10 (26) = happyGoto action_16
action_10 (27) = happyGoto action_23
action_10 (28) = happyGoto action_24
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (30) = happyShift action_19
action_11 (39) = happyShift action_20
action_11 (40) = happyShift action_21
action_11 (55) = happyShift action_13
action_11 (57) = happyShift action_22
action_11 (15) = happyGoto action_14
action_11 (17) = happyGoto action_15
action_11 (26) = happyGoto action_16
action_11 (27) = happyGoto action_17
action_11 (29) = happyGoto action_18
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (55) = happyShift action_13
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_12

action_14 _ = happyReduce_39

action_15 _ = happyReduce_36

action_16 (34) = happyShift action_66
action_16 _ = happyReduce_42

action_17 _ = happyReduce_45

action_18 (58) = happyAccept
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (30) = happyShift action_19
action_19 (39) = happyShift action_20
action_19 (40) = happyShift action_21
action_19 (47) = happyShift action_25
action_19 (55) = happyShift action_13
action_19 (57) = happyShift action_22
action_19 (15) = happyGoto action_14
action_19 (17) = happyGoto action_15
action_19 (26) = happyGoto action_16
action_19 (27) = happyGoto action_23
action_19 (28) = happyGoto action_65
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_38

action_21 _ = happyReduce_37

action_22 _ = happyReduce_14

action_23 _ = happyReduce_44

action_24 (58) = happyAccept
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (55) = happyShift action_13
action_25 (15) = happyGoto action_28
action_25 (25) = happyGoto action_64
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (58) = happyAccept
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (58) = happyAccept
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_35

action_29 (58) = happyAccept
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_16

action_31 _ = happyReduce_19

action_32 _ = happyReduce_24

action_33 (32) = happyShift action_54
action_33 (33) = happyShift action_55
action_33 _ = happyReduce_26

action_34 (30) = happyShift action_37
action_34 (45) = happyShift action_38
action_34 (50) = happyShift action_41
action_34 (53) = happyShift action_43
action_34 (55) = happyShift action_13
action_34 (56) = happyShift action_45
action_34 (15) = happyGoto action_30
action_34 (16) = happyGoto action_31
action_34 (19) = happyGoto action_32
action_34 (20) = happyGoto action_56
action_34 _ = happyReduce_31

action_35 _ = happyReduce_34

action_36 (58) = happyAccept
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (30) = happyShift action_37
action_37 (45) = happyShift action_38
action_37 (46) = happyShift action_39
action_37 (48) = happyShift action_40
action_37 (50) = happyShift action_41
action_37 (51) = happyShift action_42
action_37 (53) = happyShift action_43
action_37 (54) = happyShift action_44
action_37 (55) = happyShift action_13
action_37 (56) = happyShift action_45
action_37 (15) = happyGoto action_30
action_37 (16) = happyGoto action_31
action_37 (19) = happyGoto action_32
action_37 (20) = happyGoto action_33
action_37 (21) = happyGoto action_34
action_37 (22) = happyGoto action_46
action_37 (23) = happyGoto action_63
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_18

action_39 (55) = happyShift action_13
action_39 (15) = happyGoto action_52
action_39 (18) = happyGoto action_62
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (30) = happyShift action_37
action_40 (45) = happyShift action_38
action_40 (46) = happyShift action_39
action_40 (48) = happyShift action_40
action_40 (50) = happyShift action_41
action_40 (51) = happyShift action_42
action_40 (53) = happyShift action_43
action_40 (54) = happyShift action_44
action_40 (55) = happyShift action_13
action_40 (56) = happyShift action_45
action_40 (15) = happyGoto action_30
action_40 (16) = happyGoto action_31
action_40 (19) = happyGoto action_32
action_40 (20) = happyGoto action_33
action_40 (21) = happyGoto action_34
action_40 (22) = happyGoto action_61
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (30) = happyShift action_37
action_41 (45) = happyShift action_38
action_41 (53) = happyShift action_43
action_41 (55) = happyShift action_13
action_41 (56) = happyShift action_45
action_41 (15) = happyGoto action_30
action_41 (16) = happyGoto action_31
action_41 (19) = happyGoto action_60
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (55) = happyShift action_13
action_42 (15) = happyGoto action_52
action_42 (18) = happyGoto action_59
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_17

action_44 (55) = happyShift action_13
action_44 (15) = happyGoto action_52
action_44 (18) = happyGoto action_58
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_13

action_46 (37) = happyShift action_57
action_46 _ = happyReduce_33

action_47 (58) = happyAccept
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (58) = happyAccept
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (30) = happyShift action_37
action_49 (45) = happyShift action_38
action_49 (50) = happyShift action_41
action_49 (53) = happyShift action_43
action_49 (55) = happyShift action_13
action_49 (56) = happyShift action_45
action_49 (58) = happyAccept
action_49 (15) = happyGoto action_30
action_49 (16) = happyGoto action_31
action_49 (19) = happyGoto action_32
action_49 (20) = happyGoto action_56
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (32) = happyShift action_54
action_50 (33) = happyShift action_55
action_50 (58) = happyAccept
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (58) = happyAccept
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_15

action_53 (58) = happyAccept
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (30) = happyShift action_37
action_54 (45) = happyShift action_38
action_54 (53) = happyShift action_43
action_54 (55) = happyShift action_13
action_54 (56) = happyShift action_45
action_54 (15) = happyGoto action_30
action_54 (16) = happyGoto action_31
action_54 (19) = happyGoto action_77
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (30) = happyShift action_37
action_55 (45) = happyShift action_38
action_55 (53) = happyShift action_43
action_55 (55) = happyShift action_13
action_55 (56) = happyShift action_45
action_55 (15) = happyGoto action_30
action_55 (16) = happyGoto action_31
action_55 (19) = happyGoto action_76
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (32) = happyShift action_54
action_56 (33) = happyShift action_55
action_56 _ = happyReduce_25

action_57 (30) = happyShift action_19
action_57 (39) = happyShift action_20
action_57 (40) = happyShift action_21
action_57 (47) = happyShift action_25
action_57 (55) = happyShift action_13
action_57 (57) = happyShift action_22
action_57 (15) = happyGoto action_14
action_57 (17) = happyGoto action_15
action_57 (26) = happyGoto action_16
action_57 (27) = happyGoto action_23
action_57 (28) = happyGoto action_75
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (35) = happyShift action_74
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (38) = happyShift action_73
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_23

action_61 (52) = happyShift action_72
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (49) = happyShift action_71
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (31) = happyShift action_70
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (35) = happyShift action_69
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (31) = happyShift action_68
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (30) = happyShift action_19
action_66 (39) = happyShift action_20
action_66 (40) = happyShift action_21
action_66 (55) = happyShift action_13
action_66 (57) = happyShift action_22
action_66 (15) = happyGoto action_14
action_66 (17) = happyGoto action_15
action_66 (26) = happyGoto action_16
action_66 (27) = happyGoto action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_41

action_68 _ = happyReduce_40

action_69 (30) = happyShift action_19
action_69 (39) = happyShift action_20
action_69 (40) = happyShift action_21
action_69 (55) = happyShift action_13
action_69 (57) = happyShift action_22
action_69 (15) = happyGoto action_14
action_69 (17) = happyGoto action_15
action_69 (26) = happyGoto action_16
action_69 (27) = happyGoto action_17
action_69 (29) = happyGoto action_82
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_20

action_71 (41) = happyShift action_81
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (30) = happyShift action_37
action_72 (45) = happyShift action_38
action_72 (46) = happyShift action_39
action_72 (48) = happyShift action_40
action_72 (50) = happyShift action_41
action_72 (51) = happyShift action_42
action_72 (53) = happyShift action_43
action_72 (54) = happyShift action_44
action_72 (55) = happyShift action_13
action_72 (56) = happyShift action_45
action_72 (15) = happyGoto action_30
action_72 (16) = happyGoto action_31
action_72 (19) = happyGoto action_32
action_72 (20) = happyGoto action_33
action_72 (21) = happyGoto action_34
action_72 (22) = happyGoto action_80
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (30) = happyShift action_37
action_73 (45) = happyShift action_38
action_73 (46) = happyShift action_39
action_73 (48) = happyShift action_40
action_73 (50) = happyShift action_41
action_73 (51) = happyShift action_42
action_73 (53) = happyShift action_43
action_73 (54) = happyShift action_44
action_73 (55) = happyShift action_13
action_73 (56) = happyShift action_45
action_73 (15) = happyGoto action_30
action_73 (16) = happyGoto action_31
action_73 (19) = happyGoto action_32
action_73 (20) = happyGoto action_33
action_73 (21) = happyGoto action_34
action_73 (22) = happyGoto action_79
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (30) = happyShift action_37
action_74 (45) = happyShift action_38
action_74 (46) = happyShift action_39
action_74 (48) = happyShift action_40
action_74 (50) = happyShift action_41
action_74 (51) = happyShift action_42
action_74 (53) = happyShift action_43
action_74 (54) = happyShift action_44
action_74 (55) = happyShift action_13
action_74 (56) = happyShift action_45
action_74 (15) = happyGoto action_30
action_74 (16) = happyGoto action_31
action_74 (19) = happyGoto action_32
action_74 (20) = happyGoto action_33
action_74 (21) = happyGoto action_34
action_74 (22) = happyGoto action_35
action_74 (24) = happyGoto action_78
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_32

action_76 _ = happyReduce_22

action_77 _ = happyReduce_21

action_78 _ = happyReduce_29

action_79 (49) = happyShift action_85
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (44) = happyShift action_84
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (30) = happyShift action_37
action_81 (45) = happyShift action_38
action_81 (46) = happyShift action_39
action_81 (48) = happyShift action_40
action_81 (50) = happyShift action_41
action_81 (51) = happyShift action_42
action_81 (53) = happyShift action_43
action_81 (54) = happyShift action_44
action_81 (55) = happyShift action_13
action_81 (56) = happyShift action_45
action_81 (15) = happyGoto action_30
action_81 (16) = happyGoto action_31
action_81 (19) = happyGoto action_32
action_81 (20) = happyGoto action_33
action_81 (21) = happyGoto action_34
action_81 (22) = happyGoto action_83
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_43

action_83 (36) = happyShift action_88
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (30) = happyShift action_37
action_84 (45) = happyShift action_38
action_84 (46) = happyShift action_39
action_84 (48) = happyShift action_40
action_84 (50) = happyShift action_41
action_84 (51) = happyShift action_42
action_84 (53) = happyShift action_43
action_84 (54) = happyShift action_44
action_84 (55) = happyShift action_13
action_84 (56) = happyShift action_45
action_84 (15) = happyGoto action_30
action_84 (16) = happyGoto action_31
action_84 (19) = happyGoto action_32
action_84 (20) = happyGoto action_33
action_84 (21) = happyGoto action_34
action_84 (22) = happyGoto action_87
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (30) = happyShift action_37
action_85 (45) = happyShift action_38
action_85 (46) = happyShift action_39
action_85 (48) = happyShift action_40
action_85 (50) = happyShift action_41
action_85 (51) = happyShift action_42
action_85 (53) = happyShift action_43
action_85 (54) = happyShift action_44
action_85 (55) = happyShift action_13
action_85 (56) = happyShift action_45
action_85 (15) = happyGoto action_30
action_85 (16) = happyGoto action_31
action_85 (19) = happyGoto action_32
action_85 (20) = happyGoto action_33
action_85 (21) = happyGoto action_34
action_85 (22) = happyGoto action_35
action_85 (24) = happyGoto action_86
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_28

action_87 _ = happyReduce_27

action_88 (30) = happyShift action_37
action_88 (45) = happyShift action_38
action_88 (46) = happyShift action_39
action_88 (48) = happyShift action_40
action_88 (50) = happyShift action_41
action_88 (51) = happyShift action_42
action_88 (53) = happyShift action_43
action_88 (54) = happyShift action_44
action_88 (55) = happyShift action_13
action_88 (56) = happyShift action_45
action_88 (15) = happyGoto action_30
action_88 (16) = happyGoto action_31
action_88 (19) = happyGoto action_32
action_88 (20) = happyGoto action_33
action_88 (21) = happyGoto action_34
action_88 (22) = happyGoto action_89
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (42) = happyShift action_90
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (43) = happyShift action_91
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (30) = happyShift action_37
action_91 (45) = happyShift action_38
action_91 (46) = happyShift action_39
action_91 (48) = happyShift action_40
action_91 (50) = happyShift action_41
action_91 (51) = happyShift action_42
action_91 (53) = happyShift action_43
action_91 (54) = happyShift action_44
action_91 (55) = happyShift action_13
action_91 (56) = happyShift action_45
action_91 (15) = happyGoto action_30
action_91 (16) = happyGoto action_31
action_91 (19) = happyGoto action_32
action_91 (20) = happyGoto action_33
action_91 (21) = happyGoto action_34
action_91 (22) = happyGoto action_35
action_91 (24) = happyGoto action_92
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_30

happyReduce_12 = happySpecReduce_1  15 happyReduction_12
happyReduction_12 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn15
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.Ident happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  16 happyReduction_13
happyReduction_13 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn16
		 ((read happy_var_1) :: Integer
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  17 happyReduction_14
happyReduction_14 (HappyTerminal (PT _ (T_UVarIdent happy_var_1)))
	 =  HappyAbsSyn17
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.UVarIdent happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  18 happyReduction_15
happyReduction_15 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn18
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.PatternVar happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  19 happyReduction_16
happyReduction_16 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.EVar happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  19 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.ETrue
	)

happyReduce_18 = happySpecReduce_1  19 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.EFalse
	)

happyReduce_19 = happySpecReduce_1  19 happyReduction_19
happyReduction_19 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.ENat happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  19 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  20 happyReduction_21
happyReduction_21 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.EAdd happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  20 happyReduction_22
happyReduction_22 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.ESub happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  20 happyReduction_23
happyReduction_23 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.EIsZero happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  20 happyReduction_24
happyReduction_24 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  21 happyReduction_25
happyReduction_25 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.EApp happy_var_1 happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  21 happyReduction_26
happyReduction_26 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happyReduce 6 22 happyReduction_27
happyReduction_27 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.EIf happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 6 22 happyReduction_28
happyReduction_28 ((HappyAbsSyn24  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.ELet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 4 22 happyReduction_29
happyReduction_29 ((HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.EAbs happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 10 22 happyReduction_30
happyReduction_30 ((HappyAbsSyn24  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.EFor happy_var_2 happy_var_5 happy_var_7 happy_var_10
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_1  22 happyReduction_31
happyReduction_31 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  23 happyReduction_32
happyReduction_32 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.ETyped happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  23 happyReduction_33
happyReduction_33 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  24 happyReduction_34
happyReduction_34 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn24
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.ScopedExp happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  25 happyReduction_35
happyReduction_35 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn25
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.TPatternVar happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  26 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn26
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.TUVar happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  26 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn26
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.TNat
	)

happyReduce_38 = happySpecReduce_1  26 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn26
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.TBool
	)

happyReduce_39 = happySpecReduce_1  26 happyReduction_39
happyReduction_39 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn26
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.TVar happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  26 happyReduction_40
happyReduction_40 _
	(HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (happy_var_2
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  27 happyReduction_41
happyReduction_41 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.TArrow happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  27 happyReduction_42
happyReduction_42 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happyReduce 4 28 happyReduction_43
happyReduction_43 ((HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.TForAll happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_1  28 happyReduction_44
happyReduction_44 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  29 happyReduction_45
happyReduction_45 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn29
		 (FreeFoilTypecheck.HindleyMilner.Parser.Abs.ScopedType happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 58 58 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 30;
	PT _ (TS _ 2) -> cont 31;
	PT _ (TS _ 3) -> cont 32;
	PT _ (TS _ 4) -> cont 33;
	PT _ (TS _ 5) -> cont 34;
	PT _ (TS _ 6) -> cont 35;
	PT _ (TS _ 7) -> cont 36;
	PT _ (TS _ 8) -> cont 37;
	PT _ (TS _ 9) -> cont 38;
	PT _ (TS _ 10) -> cont 39;
	PT _ (TS _ 11) -> cont 40;
	PT _ (TS _ 12) -> cont 41;
	PT _ (TS _ 13) -> cont 42;
	PT _ (TS _ 14) -> cont 43;
	PT _ (TS _ 15) -> cont 44;
	PT _ (TS _ 16) -> cont 45;
	PT _ (TS _ 17) -> cont 46;
	PT _ (TS _ 18) -> cont 47;
	PT _ (TS _ 19) -> cont 48;
	PT _ (TS _ 20) -> cont 49;
	PT _ (TS _ 21) -> cont 50;
	PT _ (TS _ 22) -> cont 51;
	PT _ (TS _ 23) -> cont 52;
	PT _ (TS _ 24) -> cont 53;
	PT _ (TS _ 25) -> cont 54;
	PT _ (TV happy_dollar_dollar) -> cont 55;
	PT _ (TI happy_dollar_dollar) -> cont 56;
	PT _ (T_UVarIdent happy_dollar_dollar) -> cont 57;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 58 tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pExp4 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

pExp3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

pScopedExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

pTypePattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn25 z -> happyReturn z; _other -> notHappyAtAll })

pType2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

pType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

pScopedType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn29 z -> happyReturn z; _other -> notHappyAtAll })

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
