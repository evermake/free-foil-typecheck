{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module FreeFoilTypecheck.SystemF.Parser.Par
  ( happyError
  , myLexer
  , pPattern
  , pTerm3
  , pTerm2
  , pTerm1
  , pTerm
  , pScopedTerm
  ) where

import Prelude

import qualified FreeFoilTypecheck.SystemF.Parser.Abs
import FreeFoilTypecheck.SystemF.Parser.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn9 (FreeFoilTypecheck.SystemF.Parser.Abs.Ident)
	| HappyAbsSyn10 (Integer)
	| HappyAbsSyn11 (FreeFoilTypecheck.SystemF.Parser.Abs.UVarIdent)
	| HappyAbsSyn12 (FreeFoilTypecheck.SystemF.Parser.Abs.Pattern)
	| HappyAbsSyn13 (FreeFoilTypecheck.SystemF.Parser.Abs.Term)
	| HappyAbsSyn17 (FreeFoilTypecheck.SystemF.Parser.Abs.ScopedTerm)

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
 action_82 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
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
 happyReduce_36 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,432) ([0,0,0,0,7170,29186,0,7170,29250,0,7170,32478,0,7170,32478,0,7170,32478,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,24,0,0,0,0,0,0,0,0,7170,32478,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7170,32478,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15362,29250,0,0,0,0,15362,29250,0,24,0,0,0,0,0,256,0,0,7170,32478,0,7170,29186,0,7170,29186,0,24,0,0,7170,32478,0,7170,32478,0,320,0,0,320,0,0,768,0,0,7170,32478,0,15362,29506,0,320,0,0,256,32,0,4,0,0,7170,29250,0,0,0,0,0,0,0,8192,0,0,7170,32478,0,7170,32478,0,4,0,0,7170,32478,0,7170,32478,0,7170,32478,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15362,29282,0,0,0,0,15362,29251,0,0,0,0,7170,32478,0,15490,29250,0,7170,32478,0,7170,32478,0,0,0,0,0,0,0,7170,32478,0,31746,29250,0,32768,0,0,7170,32478,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pPattern","%start_pTerm3","%start_pTerm2","%start_pTerm1","%start_pTerm","%start_pScopedTerm","Ident","Integer","UVarIdent","Pattern","Term3","Term2","Term1","Term","ScopedTerm","'('","')'","'+'","'-'","'->'","'.'","'..'","':'","'='","'Bool'","'Nat'","'Type'","'['","']'","'do'","'else'","'false'","'for'","'forall'","'if'","'in'","'iszero'","'let'","'then'","'true'","'\923'","'\955'","L_Ident","L_integ","L_UVarIdent","%eof"]
        bit_start = st Prelude.* 48
        bit_end = (st Prelude.+ 1) Prelude.* 48
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..47]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (12) = happyGoto action_35
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (18) = happyShift action_15
action_1 (27) = happyShift action_16
action_1 (28) = happyShift action_17
action_1 (29) = happyShift action_18
action_1 (34) = happyShift action_19
action_1 (42) = happyShift action_25
action_1 (45) = happyShift action_7
action_1 (46) = happyShift action_28
action_1 (47) = happyShift action_29
action_1 (9) = happyGoto action_8
action_1 (10) = happyGoto action_9
action_1 (11) = happyGoto action_10
action_1 (13) = happyGoto action_34
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (18) = happyShift action_15
action_2 (27) = happyShift action_16
action_2 (28) = happyShift action_17
action_2 (29) = happyShift action_18
action_2 (34) = happyShift action_19
action_2 (39) = happyShift action_23
action_2 (42) = happyShift action_25
action_2 (45) = happyShift action_7
action_2 (46) = happyShift action_28
action_2 (47) = happyShift action_29
action_2 (9) = happyGoto action_8
action_2 (10) = happyGoto action_9
action_2 (11) = happyGoto action_10
action_2 (13) = happyGoto action_11
action_2 (14) = happyGoto action_33
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (18) = happyShift action_15
action_3 (27) = happyShift action_16
action_3 (28) = happyShift action_17
action_3 (29) = happyShift action_18
action_3 (34) = happyShift action_19
action_3 (35) = happyShift action_20
action_3 (36) = happyShift action_21
action_3 (37) = happyShift action_22
action_3 (39) = happyShift action_23
action_3 (40) = happyShift action_24
action_3 (42) = happyShift action_25
action_3 (43) = happyShift action_26
action_3 (44) = happyShift action_27
action_3 (45) = happyShift action_7
action_3 (46) = happyShift action_28
action_3 (47) = happyShift action_29
action_3 (9) = happyGoto action_8
action_3 (10) = happyGoto action_9
action_3 (11) = happyGoto action_10
action_3 (13) = happyGoto action_11
action_3 (14) = happyGoto action_12
action_3 (15) = happyGoto action_32
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (18) = happyShift action_15
action_4 (27) = happyShift action_16
action_4 (28) = happyShift action_17
action_4 (29) = happyShift action_18
action_4 (34) = happyShift action_19
action_4 (35) = happyShift action_20
action_4 (36) = happyShift action_21
action_4 (37) = happyShift action_22
action_4 (39) = happyShift action_23
action_4 (40) = happyShift action_24
action_4 (42) = happyShift action_25
action_4 (43) = happyShift action_26
action_4 (44) = happyShift action_27
action_4 (45) = happyShift action_7
action_4 (46) = happyShift action_28
action_4 (47) = happyShift action_29
action_4 (9) = happyGoto action_8
action_4 (10) = happyGoto action_9
action_4 (11) = happyGoto action_10
action_4 (13) = happyGoto action_11
action_4 (14) = happyGoto action_12
action_4 (15) = happyGoto action_30
action_4 (16) = happyGoto action_31
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (18) = happyShift action_15
action_5 (27) = happyShift action_16
action_5 (28) = happyShift action_17
action_5 (29) = happyShift action_18
action_5 (34) = happyShift action_19
action_5 (35) = happyShift action_20
action_5 (36) = happyShift action_21
action_5 (37) = happyShift action_22
action_5 (39) = happyShift action_23
action_5 (40) = happyShift action_24
action_5 (42) = happyShift action_25
action_5 (43) = happyShift action_26
action_5 (44) = happyShift action_27
action_5 (45) = happyShift action_7
action_5 (46) = happyShift action_28
action_5 (47) = happyShift action_29
action_5 (9) = happyGoto action_8
action_5 (10) = happyGoto action_9
action_5 (11) = happyGoto action_10
action_5 (13) = happyGoto action_11
action_5 (14) = happyGoto action_12
action_5 (15) = happyGoto action_13
action_5 (17) = happyGoto action_14
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (45) = happyShift action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_6

action_8 _ = happyReduce_10

action_9 _ = happyReduce_13

action_10 _ = happyReduce_14

action_11 (22) = happyShift action_50
action_11 _ = happyReduce_23

action_12 (20) = happyShift action_37
action_12 (21) = happyShift action_38
action_12 _ = happyReduce_33

action_13 (18) = happyShift action_15
action_13 (27) = happyShift action_16
action_13 (28) = happyShift action_17
action_13 (29) = happyShift action_18
action_13 (30) = happyShift action_40
action_13 (34) = happyShift action_19
action_13 (39) = happyShift action_23
action_13 (42) = happyShift action_25
action_13 (45) = happyShift action_7
action_13 (46) = happyShift action_28
action_13 (47) = happyShift action_29
action_13 (9) = happyGoto action_8
action_13 (10) = happyGoto action_9
action_13 (11) = happyGoto action_10
action_13 (13) = happyGoto action_11
action_13 (14) = happyGoto action_39
action_13 _ = happyReduce_36

action_14 (48) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (18) = happyShift action_15
action_15 (27) = happyShift action_16
action_15 (28) = happyShift action_17
action_15 (29) = happyShift action_18
action_15 (34) = happyShift action_19
action_15 (35) = happyShift action_20
action_15 (36) = happyShift action_21
action_15 (37) = happyShift action_22
action_15 (39) = happyShift action_23
action_15 (40) = happyShift action_24
action_15 (42) = happyShift action_25
action_15 (43) = happyShift action_26
action_15 (44) = happyShift action_27
action_15 (45) = happyShift action_7
action_15 (46) = happyShift action_28
action_15 (47) = happyShift action_29
action_15 (9) = happyGoto action_8
action_15 (10) = happyGoto action_9
action_15 (11) = happyGoto action_10
action_15 (13) = happyGoto action_11
action_15 (14) = happyGoto action_12
action_15 (15) = happyGoto action_30
action_15 (16) = happyGoto action_49
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_16

action_17 _ = happyReduce_15

action_18 _ = happyReduce_17

action_19 _ = happyReduce_12

action_20 (12) = happyGoto action_48
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (12) = happyGoto action_47
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (18) = happyShift action_15
action_22 (27) = happyShift action_16
action_22 (28) = happyShift action_17
action_22 (29) = happyShift action_18
action_22 (34) = happyShift action_19
action_22 (35) = happyShift action_20
action_22 (36) = happyShift action_21
action_22 (37) = happyShift action_22
action_22 (39) = happyShift action_23
action_22 (40) = happyShift action_24
action_22 (42) = happyShift action_25
action_22 (43) = happyShift action_26
action_22 (44) = happyShift action_27
action_22 (45) = happyShift action_7
action_22 (46) = happyShift action_28
action_22 (47) = happyShift action_29
action_22 (9) = happyGoto action_8
action_22 (10) = happyGoto action_9
action_22 (11) = happyGoto action_10
action_22 (13) = happyGoto action_11
action_22 (14) = happyGoto action_12
action_22 (15) = happyGoto action_46
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (18) = happyShift action_45
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (12) = happyGoto action_44
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_11

action_26 (12) = happyGoto action_43
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (12) = happyGoto action_42
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_7

action_29 _ = happyReduce_8

action_30 (18) = happyShift action_15
action_30 (25) = happyShift action_41
action_30 (27) = happyShift action_16
action_30 (28) = happyShift action_17
action_30 (29) = happyShift action_18
action_30 (30) = happyShift action_40
action_30 (34) = happyShift action_19
action_30 (39) = happyShift action_23
action_30 (42) = happyShift action_25
action_30 (45) = happyShift action_7
action_30 (46) = happyShift action_28
action_30 (47) = happyShift action_29
action_30 (9) = happyGoto action_8
action_30 (10) = happyGoto action_9
action_30 (11) = happyGoto action_10
action_30 (13) = happyGoto action_11
action_30 (14) = happyGoto action_39
action_30 _ = happyReduce_35

action_31 (48) = happyAccept
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (18) = happyShift action_15
action_32 (27) = happyShift action_16
action_32 (28) = happyShift action_17
action_32 (29) = happyShift action_18
action_32 (30) = happyShift action_40
action_32 (34) = happyShift action_19
action_32 (39) = happyShift action_23
action_32 (42) = happyShift action_25
action_32 (45) = happyShift action_7
action_32 (46) = happyShift action_28
action_32 (47) = happyShift action_29
action_32 (48) = happyAccept
action_32 (9) = happyGoto action_8
action_32 (10) = happyGoto action_9
action_32 (11) = happyGoto action_10
action_32 (13) = happyGoto action_11
action_32 (14) = happyGoto action_39
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (20) = happyShift action_37
action_33 (21) = happyShift action_38
action_33 (48) = happyAccept
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (48) = happyAccept
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (25) = happyShift action_36
action_35 (48) = happyAccept
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (18) = happyShift action_15
action_36 (27) = happyShift action_16
action_36 (28) = happyShift action_17
action_36 (29) = happyShift action_18
action_36 (34) = happyShift action_19
action_36 (35) = happyShift action_20
action_36 (36) = happyShift action_21
action_36 (37) = happyShift action_22
action_36 (39) = happyShift action_23
action_36 (40) = happyShift action_24
action_36 (42) = happyShift action_25
action_36 (43) = happyShift action_26
action_36 (44) = happyShift action_27
action_36 (45) = happyShift action_7
action_36 (46) = happyShift action_28
action_36 (47) = happyShift action_29
action_36 (9) = happyGoto action_8
action_36 (10) = happyGoto action_9
action_36 (11) = happyGoto action_10
action_36 (13) = happyGoto action_11
action_36 (14) = happyGoto action_12
action_36 (15) = happyGoto action_30
action_36 (16) = happyGoto action_64
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (18) = happyShift action_15
action_37 (27) = happyShift action_16
action_37 (28) = happyShift action_17
action_37 (29) = happyShift action_18
action_37 (34) = happyShift action_19
action_37 (42) = happyShift action_25
action_37 (45) = happyShift action_7
action_37 (46) = happyShift action_28
action_37 (47) = happyShift action_29
action_37 (9) = happyGoto action_8
action_37 (10) = happyGoto action_9
action_37 (11) = happyGoto action_10
action_37 (13) = happyGoto action_63
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (18) = happyShift action_15
action_38 (27) = happyShift action_16
action_38 (28) = happyShift action_17
action_38 (29) = happyShift action_18
action_38 (34) = happyShift action_19
action_38 (42) = happyShift action_25
action_38 (45) = happyShift action_7
action_38 (46) = happyShift action_28
action_38 (47) = happyShift action_29
action_38 (9) = happyGoto action_8
action_38 (10) = happyGoto action_9
action_38 (11) = happyGoto action_10
action_38 (13) = happyGoto action_62
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (20) = happyShift action_37
action_39 (21) = happyShift action_38
action_39 _ = happyReduce_28

action_40 (18) = happyShift action_15
action_40 (27) = happyShift action_16
action_40 (28) = happyShift action_17
action_40 (29) = happyShift action_18
action_40 (34) = happyShift action_19
action_40 (35) = happyShift action_20
action_40 (36) = happyShift action_21
action_40 (37) = happyShift action_22
action_40 (39) = happyShift action_23
action_40 (40) = happyShift action_24
action_40 (42) = happyShift action_25
action_40 (43) = happyShift action_26
action_40 (44) = happyShift action_27
action_40 (45) = happyShift action_7
action_40 (46) = happyShift action_28
action_40 (47) = happyShift action_29
action_40 (9) = happyGoto action_8
action_40 (10) = happyGoto action_9
action_40 (11) = happyGoto action_10
action_40 (13) = happyGoto action_11
action_40 (14) = happyGoto action_12
action_40 (15) = happyGoto action_30
action_40 (16) = happyGoto action_61
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (18) = happyShift action_15
action_41 (27) = happyShift action_16
action_41 (28) = happyShift action_17
action_41 (29) = happyShift action_18
action_41 (34) = happyShift action_19
action_41 (35) = happyShift action_20
action_41 (36) = happyShift action_21
action_41 (37) = happyShift action_22
action_41 (39) = happyShift action_23
action_41 (40) = happyShift action_24
action_41 (42) = happyShift action_25
action_41 (43) = happyShift action_26
action_41 (44) = happyShift action_27
action_41 (45) = happyShift action_7
action_41 (46) = happyShift action_28
action_41 (47) = happyShift action_29
action_41 (9) = happyGoto action_8
action_41 (10) = happyGoto action_9
action_41 (11) = happyGoto action_10
action_41 (13) = happyGoto action_11
action_41 (14) = happyGoto action_12
action_41 (15) = happyGoto action_30
action_41 (16) = happyGoto action_60
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (23) = happyShift action_59
action_42 (25) = happyShift action_36
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (23) = happyShift action_58
action_43 (25) = happyShift action_36
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (25) = happyShift action_36
action_44 (26) = happyShift action_57
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (18) = happyShift action_15
action_45 (27) = happyShift action_16
action_45 (28) = happyShift action_17
action_45 (29) = happyShift action_18
action_45 (34) = happyShift action_19
action_45 (35) = happyShift action_20
action_45 (36) = happyShift action_21
action_45 (37) = happyShift action_22
action_45 (39) = happyShift action_23
action_45 (40) = happyShift action_24
action_45 (42) = happyShift action_25
action_45 (43) = happyShift action_26
action_45 (44) = happyShift action_27
action_45 (45) = happyShift action_7
action_45 (46) = happyShift action_28
action_45 (47) = happyShift action_29
action_45 (9) = happyGoto action_8
action_45 (10) = happyGoto action_9
action_45 (11) = happyGoto action_10
action_45 (13) = happyGoto action_11
action_45 (14) = happyGoto action_12
action_45 (15) = happyGoto action_30
action_45 (16) = happyGoto action_56
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (18) = happyShift action_15
action_46 (27) = happyShift action_16
action_46 (28) = happyShift action_17
action_46 (29) = happyShift action_18
action_46 (30) = happyShift action_40
action_46 (34) = happyShift action_19
action_46 (39) = happyShift action_23
action_46 (41) = happyShift action_55
action_46 (42) = happyShift action_25
action_46 (45) = happyShift action_7
action_46 (46) = happyShift action_28
action_46 (47) = happyShift action_29
action_46 (9) = happyGoto action_8
action_46 (10) = happyGoto action_9
action_46 (11) = happyGoto action_10
action_46 (13) = happyGoto action_11
action_46 (14) = happyGoto action_39
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (23) = happyShift action_54
action_47 (25) = happyShift action_36
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (25) = happyShift action_36
action_48 (38) = happyShift action_53
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (19) = happyShift action_52
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (18) = happyShift action_15
action_50 (27) = happyShift action_16
action_50 (28) = happyShift action_17
action_50 (29) = happyShift action_18
action_50 (34) = happyShift action_19
action_50 (39) = happyShift action_23
action_50 (42) = happyShift action_25
action_50 (45) = happyShift action_7
action_50 (46) = happyShift action_28
action_50 (47) = happyShift action_29
action_50 (9) = happyGoto action_8
action_50 (10) = happyGoto action_9
action_50 (11) = happyGoto action_10
action_50 (13) = happyGoto action_11
action_50 (14) = happyGoto action_51
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (20) = happyShift action_37
action_51 (21) = happyShift action_38
action_51 _ = happyReduce_22

action_52 _ = happyReduce_18

action_53 (30) = happyShift action_72
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (18) = happyShift action_15
action_54 (27) = happyShift action_16
action_54 (28) = happyShift action_17
action_54 (29) = happyShift action_18
action_54 (34) = happyShift action_19
action_54 (35) = happyShift action_20
action_54 (36) = happyShift action_21
action_54 (37) = happyShift action_22
action_54 (39) = happyShift action_23
action_54 (40) = happyShift action_24
action_54 (42) = happyShift action_25
action_54 (43) = happyShift action_26
action_54 (44) = happyShift action_27
action_54 (45) = happyShift action_7
action_54 (46) = happyShift action_28
action_54 (47) = happyShift action_29
action_54 (9) = happyGoto action_8
action_54 (10) = happyGoto action_9
action_54 (11) = happyGoto action_10
action_54 (13) = happyGoto action_11
action_54 (14) = happyGoto action_12
action_54 (15) = happyGoto action_13
action_54 (17) = happyGoto action_71
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (18) = happyShift action_15
action_55 (27) = happyShift action_16
action_55 (28) = happyShift action_17
action_55 (29) = happyShift action_18
action_55 (34) = happyShift action_19
action_55 (35) = happyShift action_20
action_55 (36) = happyShift action_21
action_55 (37) = happyShift action_22
action_55 (39) = happyShift action_23
action_55 (40) = happyShift action_24
action_55 (42) = happyShift action_25
action_55 (43) = happyShift action_26
action_55 (44) = happyShift action_27
action_55 (45) = happyShift action_7
action_55 (46) = happyShift action_28
action_55 (47) = happyShift action_29
action_55 (9) = happyGoto action_8
action_55 (10) = happyGoto action_9
action_55 (11) = happyGoto action_10
action_55 (13) = happyGoto action_11
action_55 (14) = happyGoto action_12
action_55 (15) = happyGoto action_70
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (19) = happyShift action_69
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (18) = happyShift action_15
action_57 (27) = happyShift action_16
action_57 (28) = happyShift action_17
action_57 (29) = happyShift action_18
action_57 (34) = happyShift action_19
action_57 (35) = happyShift action_20
action_57 (36) = happyShift action_21
action_57 (37) = happyShift action_22
action_57 (39) = happyShift action_23
action_57 (40) = happyShift action_24
action_57 (42) = happyShift action_25
action_57 (43) = happyShift action_26
action_57 (44) = happyShift action_27
action_57 (45) = happyShift action_7
action_57 (46) = happyShift action_28
action_57 (47) = happyShift action_29
action_57 (9) = happyGoto action_8
action_57 (10) = happyGoto action_9
action_57 (11) = happyGoto action_10
action_57 (13) = happyGoto action_11
action_57 (14) = happyGoto action_12
action_57 (15) = happyGoto action_68
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (18) = happyShift action_15
action_58 (27) = happyShift action_16
action_58 (28) = happyShift action_17
action_58 (29) = happyShift action_18
action_58 (34) = happyShift action_19
action_58 (35) = happyShift action_20
action_58 (36) = happyShift action_21
action_58 (37) = happyShift action_22
action_58 (39) = happyShift action_23
action_58 (40) = happyShift action_24
action_58 (42) = happyShift action_25
action_58 (43) = happyShift action_26
action_58 (44) = happyShift action_27
action_58 (45) = happyShift action_7
action_58 (46) = happyShift action_28
action_58 (47) = happyShift action_29
action_58 (9) = happyGoto action_8
action_58 (10) = happyGoto action_9
action_58 (11) = happyGoto action_10
action_58 (13) = happyGoto action_11
action_58 (14) = happyGoto action_12
action_58 (15) = happyGoto action_13
action_58 (17) = happyGoto action_67
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (18) = happyShift action_15
action_59 (27) = happyShift action_16
action_59 (28) = happyShift action_17
action_59 (29) = happyShift action_18
action_59 (34) = happyShift action_19
action_59 (35) = happyShift action_20
action_59 (36) = happyShift action_21
action_59 (37) = happyShift action_22
action_59 (39) = happyShift action_23
action_59 (40) = happyShift action_24
action_59 (42) = happyShift action_25
action_59 (43) = happyShift action_26
action_59 (44) = happyShift action_27
action_59 (45) = happyShift action_7
action_59 (46) = happyShift action_28
action_59 (47) = happyShift action_29
action_59 (9) = happyGoto action_8
action_59 (10) = happyGoto action_9
action_59 (11) = happyGoto action_10
action_59 (13) = happyGoto action_11
action_59 (14) = happyGoto action_12
action_59 (15) = happyGoto action_13
action_59 (17) = happyGoto action_66
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_34

action_61 (31) = happyShift action_65
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_20

action_63 _ = happyReduce_19

action_64 _ = happyReduce_9

action_65 _ = happyReduce_30

action_66 (18) = happyReduce_27
action_66 (19) = happyReduce_27
action_66 (23) = happyReduce_27
action_66 (24) = happyReduce_27
action_66 (25) = happyReduce_27
action_66 (26) = happyReduce_27
action_66 (27) = happyReduce_27
action_66 (28) = happyReduce_27
action_66 (29) = happyReduce_27
action_66 (30) = happyReduce_27
action_66 (31) = happyReduce_27
action_66 (33) = happyReduce_27
action_66 (34) = happyReduce_27
action_66 (38) = happyReduce_27
action_66 (39) = happyReduce_27
action_66 (41) = happyReduce_27
action_66 (42) = happyReduce_27
action_66 (45) = happyReduce_27
action_66 (46) = happyReduce_27
action_66 (47) = happyReduce_27
action_66 (48) = happyReduce_27
action_66 _ = happyReduce_27

action_67 _ = happyReduce_29

action_68 (18) = happyShift action_15
action_68 (27) = happyShift action_16
action_68 (28) = happyShift action_17
action_68 (29) = happyShift action_18
action_68 (30) = happyShift action_40
action_68 (34) = happyShift action_19
action_68 (38) = happyShift action_75
action_68 (39) = happyShift action_23
action_68 (42) = happyShift action_25
action_68 (45) = happyShift action_7
action_68 (46) = happyShift action_28
action_68 (47) = happyShift action_29
action_68 (9) = happyGoto action_8
action_68 (10) = happyGoto action_9
action_68 (11) = happyGoto action_10
action_68 (13) = happyGoto action_11
action_68 (14) = happyGoto action_39
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_21

action_70 (18) = happyShift action_15
action_70 (27) = happyShift action_16
action_70 (28) = happyShift action_17
action_70 (29) = happyShift action_18
action_70 (30) = happyShift action_40
action_70 (33) = happyShift action_74
action_70 (34) = happyShift action_19
action_70 (39) = happyShift action_23
action_70 (42) = happyShift action_25
action_70 (45) = happyShift action_7
action_70 (46) = happyShift action_28
action_70 (47) = happyShift action_29
action_70 (9) = happyGoto action_8
action_70 (10) = happyGoto action_9
action_70 (11) = happyGoto action_10
action_70 (13) = happyGoto action_11
action_70 (14) = happyGoto action_39
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_32

action_72 (18) = happyShift action_15
action_72 (27) = happyShift action_16
action_72 (28) = happyShift action_17
action_72 (29) = happyShift action_18
action_72 (34) = happyShift action_19
action_72 (35) = happyShift action_20
action_72 (36) = happyShift action_21
action_72 (37) = happyShift action_22
action_72 (39) = happyShift action_23
action_72 (40) = happyShift action_24
action_72 (42) = happyShift action_25
action_72 (43) = happyShift action_26
action_72 (44) = happyShift action_27
action_72 (45) = happyShift action_7
action_72 (46) = happyShift action_28
action_72 (47) = happyShift action_29
action_72 (9) = happyGoto action_8
action_72 (10) = happyGoto action_9
action_72 (11) = happyGoto action_10
action_72 (13) = happyGoto action_11
action_72 (14) = happyGoto action_12
action_72 (15) = happyGoto action_73
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (18) = happyShift action_15
action_73 (24) = happyShift action_78
action_73 (27) = happyShift action_16
action_73 (28) = happyShift action_17
action_73 (29) = happyShift action_18
action_73 (30) = happyShift action_40
action_73 (34) = happyShift action_19
action_73 (39) = happyShift action_23
action_73 (42) = happyShift action_25
action_73 (45) = happyShift action_7
action_73 (46) = happyShift action_28
action_73 (47) = happyShift action_29
action_73 (9) = happyGoto action_8
action_73 (10) = happyGoto action_9
action_73 (11) = happyGoto action_10
action_73 (13) = happyGoto action_11
action_73 (14) = happyGoto action_39
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (18) = happyShift action_15
action_74 (27) = happyShift action_16
action_74 (28) = happyShift action_17
action_74 (29) = happyShift action_18
action_74 (34) = happyShift action_19
action_74 (35) = happyShift action_20
action_74 (36) = happyShift action_21
action_74 (37) = happyShift action_22
action_74 (39) = happyShift action_23
action_74 (40) = happyShift action_24
action_74 (42) = happyShift action_25
action_74 (43) = happyShift action_26
action_74 (44) = happyShift action_27
action_74 (45) = happyShift action_7
action_74 (46) = happyShift action_28
action_74 (47) = happyShift action_29
action_74 (9) = happyGoto action_8
action_74 (10) = happyGoto action_9
action_74 (11) = happyGoto action_10
action_74 (13) = happyGoto action_11
action_74 (14) = happyGoto action_12
action_74 (15) = happyGoto action_77
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (18) = happyShift action_15
action_75 (27) = happyShift action_16
action_75 (28) = happyShift action_17
action_75 (29) = happyShift action_18
action_75 (34) = happyShift action_19
action_75 (35) = happyShift action_20
action_75 (36) = happyShift action_21
action_75 (37) = happyShift action_22
action_75 (39) = happyShift action_23
action_75 (40) = happyShift action_24
action_75 (42) = happyShift action_25
action_75 (43) = happyShift action_26
action_75 (44) = happyShift action_27
action_75 (45) = happyShift action_7
action_75 (46) = happyShift action_28
action_75 (47) = happyShift action_29
action_75 (9) = happyGoto action_8
action_75 (10) = happyGoto action_9
action_75 (11) = happyGoto action_10
action_75 (13) = happyGoto action_11
action_75 (14) = happyGoto action_12
action_75 (15) = happyGoto action_13
action_75 (17) = happyGoto action_76
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_25

action_77 (18) = happyShift action_15
action_77 (27) = happyShift action_16
action_77 (28) = happyShift action_17
action_77 (29) = happyShift action_18
action_77 (30) = happyShift action_40
action_77 (34) = happyShift action_19
action_77 (39) = happyShift action_23
action_77 (42) = happyShift action_25
action_77 (45) = happyShift action_7
action_77 (46) = happyShift action_28
action_77 (47) = happyShift action_29
action_77 (9) = happyGoto action_8
action_77 (10) = happyGoto action_9
action_77 (11) = happyGoto action_10
action_77 (13) = happyGoto action_11
action_77 (14) = happyGoto action_39
action_77 _ = happyReduce_24

action_78 (18) = happyShift action_15
action_78 (27) = happyShift action_16
action_78 (28) = happyShift action_17
action_78 (29) = happyShift action_18
action_78 (34) = happyShift action_19
action_78 (35) = happyShift action_20
action_78 (36) = happyShift action_21
action_78 (37) = happyShift action_22
action_78 (39) = happyShift action_23
action_78 (40) = happyShift action_24
action_78 (42) = happyShift action_25
action_78 (43) = happyShift action_26
action_78 (44) = happyShift action_27
action_78 (45) = happyShift action_7
action_78 (46) = happyShift action_28
action_78 (47) = happyShift action_29
action_78 (9) = happyGoto action_8
action_78 (10) = happyGoto action_9
action_78 (11) = happyGoto action_10
action_78 (13) = happyGoto action_11
action_78 (14) = happyGoto action_12
action_78 (15) = happyGoto action_79
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (18) = happyShift action_15
action_79 (27) = happyShift action_16
action_79 (28) = happyShift action_17
action_79 (29) = happyShift action_18
action_79 (30) = happyShift action_40
action_79 (31) = happyShift action_80
action_79 (34) = happyShift action_19
action_79 (39) = happyShift action_23
action_79 (42) = happyShift action_25
action_79 (45) = happyShift action_7
action_79 (46) = happyShift action_28
action_79 (47) = happyShift action_29
action_79 (9) = happyGoto action_8
action_79 (10) = happyGoto action_9
action_79 (11) = happyGoto action_10
action_79 (13) = happyGoto action_11
action_79 (14) = happyGoto action_39
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (32) = happyShift action_81
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (18) = happyShift action_15
action_81 (27) = happyShift action_16
action_81 (28) = happyShift action_17
action_81 (29) = happyShift action_18
action_81 (34) = happyShift action_19
action_81 (35) = happyShift action_20
action_81 (36) = happyShift action_21
action_81 (37) = happyShift action_22
action_81 (39) = happyShift action_23
action_81 (40) = happyShift action_24
action_81 (42) = happyShift action_25
action_81 (43) = happyShift action_26
action_81 (44) = happyShift action_27
action_81 (45) = happyShift action_7
action_81 (46) = happyShift action_28
action_81 (47) = happyShift action_29
action_81 (9) = happyGoto action_8
action_81 (10) = happyGoto action_9
action_81 (11) = happyGoto action_10
action_81 (13) = happyGoto action_11
action_81 (14) = happyGoto action_12
action_81 (15) = happyGoto action_13
action_81 (17) = happyGoto action_82
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_31

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn9
		 (FreeFoilTypecheck.SystemF.Parser.Abs.Ident happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  10 happyReduction_7
happyReduction_7 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn10
		 ((read happy_var_1) :: Integer
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  11 happyReduction_8
happyReduction_8 (HappyTerminal (PT _ (T_UVarIdent happy_var_1)))
	 =  HappyAbsSyn11
		 (FreeFoilTypecheck.SystemF.Parser.Abs.UVarIdent happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  12 happyReduction_9
happyReduction_9 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (FreeFoilTypecheck.SystemF.Parser.Abs.PatternAsc happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  13 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.EVar happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  13 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.ETrue
	)

happyReduce_12 = happySpecReduce_1  13 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.EFalse
	)

happyReduce_13 = happySpecReduce_1  13 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.ENat happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  13 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.TUVar happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  13 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.TNat
	)

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.TBool
	)

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.TType
	)

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  14 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.EAdd happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  14 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.ESub happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 14 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.EIsZero happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_3  14 happyReduction_22
happyReduction_22 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.TArrow happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happyReduce 6 15 happyReduction_24
happyReduction_24 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.EIf happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 6 15 happyReduction_25
happyReduction_25 ((HappyAbsSyn17  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.ELet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 4 15 happyReduction_26
happyReduction_26 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.EAbsTyped happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 4 15 happyReduction_27
happyReduction_27 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.EAbsUntyped happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_2  15 happyReduction_28
happyReduction_28 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.EApp happy_var_1 happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happyReduce 4 15 happyReduction_29
happyReduction_29 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.ETAbs happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 4 15 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.ETApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 10 15 happyReduction_31
happyReduction_31 ((HappyAbsSyn17  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.EFor happy_var_2 happy_var_5 happy_var_7 happy_var_10
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 4 15 happyReduction_32
happyReduction_32 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.TForAll happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_1  15 happyReduction_33
happyReduction_33 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  16 happyReduction_34
happyReduction_34 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (FreeFoilTypecheck.SystemF.Parser.Abs.ETyped happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  16 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  17 happyReduction_36
happyReduction_36 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn17
		 (FreeFoilTypecheck.SystemF.Parser.Abs.ScopedTerm happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 48 48 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 18;
	PT _ (TS _ 2) -> cont 19;
	PT _ (TS _ 3) -> cont 20;
	PT _ (TS _ 4) -> cont 21;
	PT _ (TS _ 5) -> cont 22;
	PT _ (TS _ 6) -> cont 23;
	PT _ (TS _ 7) -> cont 24;
	PT _ (TS _ 8) -> cont 25;
	PT _ (TS _ 9) -> cont 26;
	PT _ (TS _ 10) -> cont 27;
	PT _ (TS _ 11) -> cont 28;
	PT _ (TS _ 12) -> cont 29;
	PT _ (TS _ 13) -> cont 30;
	PT _ (TS _ 14) -> cont 31;
	PT _ (TS _ 15) -> cont 32;
	PT _ (TS _ 16) -> cont 33;
	PT _ (TS _ 17) -> cont 34;
	PT _ (TS _ 18) -> cont 35;
	PT _ (TS _ 19) -> cont 36;
	PT _ (TS _ 20) -> cont 37;
	PT _ (TS _ 21) -> cont 38;
	PT _ (TS _ 22) -> cont 39;
	PT _ (TS _ 23) -> cont 40;
	PT _ (TS _ 24) -> cont 41;
	PT _ (TS _ 25) -> cont 42;
	PT _ (TS _ 26) -> cont 43;
	PT _ (TS _ 27) -> cont 44;
	PT _ (TV happy_dollar_dollar) -> cont 45;
	PT _ (TI happy_dollar_dollar) -> cont 46;
	PT _ (T_UVarIdent happy_dollar_dollar) -> cont 47;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 48 tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn12 z -> happyReturn z; _other -> notHappyAtAll })

pTerm3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

pTerm2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

pTerm1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

pTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

pScopedTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

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
