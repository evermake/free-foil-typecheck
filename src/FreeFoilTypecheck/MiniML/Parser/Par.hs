{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module FreeFoilTypecheck.MiniML.Parser.Par
  ( happyError
  , myLexer
  , pPattern2
  , pPattern1
  , pPattern
  , pExp6
  , pExp5
  , pExp4
  , pExp3
  , pExp1
  , pExp
  , pExp2
  , pBranch
  , pListBranch
  , pScopedExp
  , pTypePattern
  , pType5
  , pType4
  , pType3
  , pType2
  , pType1
  , pType
  , pScopedType
  ) where

import Prelude

import qualified FreeFoilTypecheck.MiniML.Parser.Abs
import FreeFoilTypecheck.MiniML.Parser.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn24 (FreeFoilTypecheck.MiniML.Parser.Abs.Ident)
	| HappyAbsSyn25 (Integer)
	| HappyAbsSyn26 (FreeFoilTypecheck.MiniML.Parser.Abs.UVarIdent)
	| HappyAbsSyn27 (FreeFoilTypecheck.MiniML.Parser.Abs.Pattern)
	| HappyAbsSyn30 (FreeFoilTypecheck.MiniML.Parser.Abs.Exp)
	| HappyAbsSyn37 (FreeFoilTypecheck.MiniML.Parser.Abs.Branch)
	| HappyAbsSyn38 ([FreeFoilTypecheck.MiniML.Parser.Abs.Branch])
	| HappyAbsSyn39 (FreeFoilTypecheck.MiniML.Parser.Abs.ScopedExp)
	| HappyAbsSyn40 (FreeFoilTypecheck.MiniML.Parser.Abs.TypePattern)
	| HappyAbsSyn41 (FreeFoilTypecheck.MiniML.Parser.Abs.Type)
	| HappyAbsSyn47 (FreeFoilTypecheck.MiniML.Parser.Abs.ScopedType)

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
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

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
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,591) ([0,0,32768,40960,0,32,0,0,256,320,16390,0,0,0,32770,3074,128,0,0,1024,8448,2048,3,0,0,8,29250,1556,0,0,4096,33792,10468,12,0,0,32,51464,6225,0,0,16384,36864,44982,56,0,0,128,27936,29023,0,0,0,16385,36424,194,0,0,512,640,32780,0,0,0,4,6149,256,0,0,2048,53760,5622,7,0,0,0,0,1024,0,0,8192,1280,0,40,0,0,64,14,20480,0,0,32768,7168,0,160,0,0,256,56,16384,1,0,0,28674,0,640,0,0,1024,32992,0,5,0,0,49160,1,2560,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,32,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,2062,20480,0,0,0,0,0,0,0,0,256,40,16384,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,2112,49664,0,0,0,1104,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,36864,44982,56,0,0,0,64,0,0,0,0,16385,48858,226,0,0,0,0,0,0,0,0,4,6149,256,0,0,2048,16896,4096,6,0,0,16,60836,3627,0,0,8192,2048,16385,24,0,0,64,528,12416,0,0,32768,8192,4,97,0,0,256,320,16390,0,0,0,32770,3074,128,0,0,1024,8448,2048,3,0,0,0,0,0,0,0,4096,5120,96,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,32768,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,16,24596,1024,0,0,0,4096,0,0,0,0,0,0,0,0,0,32768,40960,0,32,0,0,256,320,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5120,0,0,0,0,0,16385,8,194,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,37392,12451,0,0,32768,8192,18212,97,0,0,256,8248,16384,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,1,0,0,0,0,32,49192,2048,0,0,16384,36864,44982,56,0,0,128,160,8195,0,0,0,128,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,17,0,0,0,0,64,0,0,0,0,16384,4096,41874,48,0,0,16384,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,57348,0,1280,0,0,2048,448,0,10,0,0,32784,3,5120,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,28674,0,640,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,41984,11245,14,0,0,0,0,128,0,0,16384,36864,44982,56,0,0,128,27936,29023,0,0,0,16385,48858,226,0,0,512,46208,50557,1,0,0,4,64361,906,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,40960,768,32,0,0,0,0,0,0,0,0,32770,16,388,0,0,1024,8448,2048,3,0,0,16,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,64,0,0,0,0,512,0,0,0,0,0,0,0,0,0,512,640,32780,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,64,46736,14511,0,0,32768,8192,24429,113,0,0,256,55872,58046,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pPattern2","%start_pPattern1","%start_pPattern","%start_pExp6","%start_pExp5","%start_pExp4","%start_pExp3","%start_pExp1","%start_pExp","%start_pExp2","%start_pBranch","%start_pListBranch","%start_pScopedExp","%start_pTypePattern","%start_pType5","%start_pType4","%start_pType3","%start_pType2","%start_pType1","%start_pType","%start_pScopedType","Ident","Integer","UVarIdent","Pattern2","Pattern1","Pattern","Exp6","Exp5","Exp4","Exp3","Exp1","Exp","Exp2","Branch","ListBranch","ScopedExp","TypePattern","Type5","Type4","Type3","Type2","Type1","Type","ScopedType","'('","')'","'*'","'+'","','","'-'","'->'","'.'","':'","'::'","'='","'Bool'","'List'","'Nat'","'['","']'","'_'","'case'","'else'","'false'","'fix'","'forall'","'fst'","'if'","'in'","'inl'","'inr'","'iszero'","'let'","'letrec'","'of'","'snd'","'then'","'true'","'{'","'|'","'}'","'\955'","L_Ident","L_integ","L_UVarIdent","%eof"]
        bit_start = st Prelude.* 89
        bit_end = (st Prelude.+ 1) Prelude.* 89
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..88]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (48) = happyShift action_77
action_0 (62) = happyShift action_78
action_0 (64) = happyShift action_79
action_0 (86) = happyShift action_22
action_0 (24) = happyGoto action_71
action_0 (27) = happyGoto action_93
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (48) = happyShift action_77
action_1 (62) = happyShift action_78
action_1 (64) = happyShift action_79
action_1 (73) = happyShift action_80
action_1 (74) = happyShift action_81
action_1 (86) = happyShift action_22
action_1 (24) = happyGoto action_71
action_1 (27) = happyGoto action_72
action_1 (28) = happyGoto action_92
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (48) = happyShift action_77
action_2 (62) = happyShift action_78
action_2 (64) = happyShift action_79
action_2 (73) = happyShift action_80
action_2 (74) = happyShift action_81
action_2 (86) = happyShift action_22
action_2 (24) = happyGoto action_71
action_2 (27) = happyGoto action_72
action_2 (28) = happyGoto action_73
action_2 (29) = happyGoto action_91
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (48) = happyShift action_55
action_3 (62) = happyShift action_56
action_3 (67) = happyShift action_58
action_3 (81) = happyShift action_68
action_3 (86) = happyShift action_22
action_3 (87) = happyShift action_70
action_3 (24) = happyGoto action_46
action_3 (25) = happyGoto action_47
action_3 (30) = happyGoto action_90
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (48) = happyShift action_55
action_4 (62) = happyShift action_56
action_4 (67) = happyShift action_58
action_4 (70) = happyShift action_60
action_4 (73) = happyShift action_62
action_4 (74) = happyShift action_63
action_4 (75) = happyShift action_64
action_4 (79) = happyShift action_67
action_4 (81) = happyShift action_68
action_4 (86) = happyShift action_22
action_4 (87) = happyShift action_70
action_4 (24) = happyGoto action_46
action_4 (25) = happyGoto action_47
action_4 (30) = happyGoto action_48
action_4 (31) = happyGoto action_89
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (48) = happyShift action_55
action_5 (62) = happyShift action_56
action_5 (67) = happyShift action_58
action_5 (70) = happyShift action_60
action_5 (73) = happyShift action_62
action_5 (74) = happyShift action_63
action_5 (75) = happyShift action_64
action_5 (79) = happyShift action_67
action_5 (81) = happyShift action_68
action_5 (86) = happyShift action_22
action_5 (87) = happyShift action_70
action_5 (24) = happyGoto action_46
action_5 (25) = happyGoto action_47
action_5 (30) = happyGoto action_48
action_5 (31) = happyGoto action_49
action_5 (32) = happyGoto action_88
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (48) = happyShift action_55
action_6 (62) = happyShift action_56
action_6 (67) = happyShift action_58
action_6 (70) = happyShift action_60
action_6 (73) = happyShift action_62
action_6 (74) = happyShift action_63
action_6 (75) = happyShift action_64
action_6 (79) = happyShift action_67
action_6 (81) = happyShift action_68
action_6 (86) = happyShift action_22
action_6 (87) = happyShift action_70
action_6 (24) = happyGoto action_46
action_6 (25) = happyGoto action_47
action_6 (30) = happyGoto action_48
action_6 (31) = happyGoto action_49
action_6 (32) = happyGoto action_50
action_6 (33) = happyGoto action_87
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (48) = happyShift action_55
action_7 (62) = happyShift action_56
action_7 (65) = happyShift action_57
action_7 (67) = happyShift action_58
action_7 (68) = happyShift action_59
action_7 (70) = happyShift action_60
action_7 (71) = happyShift action_61
action_7 (73) = happyShift action_62
action_7 (74) = happyShift action_63
action_7 (75) = happyShift action_64
action_7 (76) = happyShift action_65
action_7 (77) = happyShift action_66
action_7 (79) = happyShift action_67
action_7 (81) = happyShift action_68
action_7 (85) = happyShift action_69
action_7 (86) = happyShift action_22
action_7 (87) = happyShift action_70
action_7 (24) = happyGoto action_46
action_7 (25) = happyGoto action_47
action_7 (30) = happyGoto action_48
action_7 (31) = happyGoto action_49
action_7 (32) = happyGoto action_50
action_7 (33) = happyGoto action_51
action_7 (34) = happyGoto action_86
action_7 (36) = happyGoto action_53
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (48) = happyShift action_55
action_8 (62) = happyShift action_56
action_8 (65) = happyShift action_57
action_8 (67) = happyShift action_58
action_8 (68) = happyShift action_59
action_8 (70) = happyShift action_60
action_8 (71) = happyShift action_61
action_8 (73) = happyShift action_62
action_8 (74) = happyShift action_63
action_8 (75) = happyShift action_64
action_8 (76) = happyShift action_65
action_8 (77) = happyShift action_66
action_8 (79) = happyShift action_67
action_8 (81) = happyShift action_68
action_8 (85) = happyShift action_69
action_8 (86) = happyShift action_22
action_8 (87) = happyShift action_70
action_8 (24) = happyGoto action_46
action_8 (25) = happyGoto action_47
action_8 (30) = happyGoto action_48
action_8 (31) = happyGoto action_49
action_8 (32) = happyGoto action_50
action_8 (33) = happyGoto action_51
action_8 (34) = happyGoto action_84
action_8 (35) = happyGoto action_85
action_8 (36) = happyGoto action_53
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (48) = happyShift action_55
action_9 (62) = happyShift action_56
action_9 (67) = happyShift action_58
action_9 (70) = happyShift action_60
action_9 (73) = happyShift action_62
action_9 (74) = happyShift action_63
action_9 (75) = happyShift action_64
action_9 (79) = happyShift action_67
action_9 (81) = happyShift action_68
action_9 (86) = happyShift action_22
action_9 (87) = happyShift action_70
action_9 (24) = happyGoto action_46
action_9 (25) = happyGoto action_47
action_9 (30) = happyGoto action_48
action_9 (31) = happyGoto action_49
action_9 (32) = happyGoto action_50
action_9 (33) = happyGoto action_51
action_9 (36) = happyGoto action_83
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (48) = happyShift action_77
action_10 (62) = happyShift action_78
action_10 (64) = happyShift action_79
action_10 (73) = happyShift action_80
action_10 (74) = happyShift action_81
action_10 (86) = happyShift action_22
action_10 (24) = happyGoto action_71
action_10 (27) = happyGoto action_72
action_10 (28) = happyGoto action_73
action_10 (29) = happyGoto action_74
action_10 (37) = happyGoto action_82
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (48) = happyShift action_77
action_11 (62) = happyShift action_78
action_11 (64) = happyShift action_79
action_11 (73) = happyShift action_80
action_11 (74) = happyShift action_81
action_11 (86) = happyShift action_22
action_11 (24) = happyGoto action_71
action_11 (27) = happyGoto action_72
action_11 (28) = happyGoto action_73
action_11 (29) = happyGoto action_74
action_11 (37) = happyGoto action_75
action_11 (38) = happyGoto action_76
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (48) = happyShift action_55
action_12 (62) = happyShift action_56
action_12 (65) = happyShift action_57
action_12 (67) = happyShift action_58
action_12 (68) = happyShift action_59
action_12 (70) = happyShift action_60
action_12 (71) = happyShift action_61
action_12 (73) = happyShift action_62
action_12 (74) = happyShift action_63
action_12 (75) = happyShift action_64
action_12 (76) = happyShift action_65
action_12 (77) = happyShift action_66
action_12 (79) = happyShift action_67
action_12 (81) = happyShift action_68
action_12 (85) = happyShift action_69
action_12 (86) = happyShift action_22
action_12 (87) = happyShift action_70
action_12 (24) = happyGoto action_46
action_12 (25) = happyGoto action_47
action_12 (30) = happyGoto action_48
action_12 (31) = happyGoto action_49
action_12 (32) = happyGoto action_50
action_12 (33) = happyGoto action_51
action_12 (34) = happyGoto action_52
action_12 (36) = happyGoto action_53
action_12 (39) = happyGoto action_54
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (86) = happyShift action_22
action_13 (24) = happyGoto action_44
action_13 (40) = happyGoto action_45
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (48) = happyShift action_31
action_14 (59) = happyShift action_32
action_14 (61) = happyShift action_34
action_14 (86) = happyShift action_22
action_14 (88) = happyShift action_35
action_14 (24) = happyGoto action_23
action_14 (26) = happyGoto action_24
action_14 (41) = happyGoto action_43
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (48) = happyShift action_31
action_15 (59) = happyShift action_32
action_15 (60) = happyShift action_33
action_15 (61) = happyShift action_34
action_15 (86) = happyShift action_22
action_15 (88) = happyShift action_35
action_15 (24) = happyGoto action_23
action_15 (26) = happyGoto action_24
action_15 (41) = happyGoto action_25
action_15 (42) = happyGoto action_42
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (48) = happyShift action_31
action_16 (59) = happyShift action_32
action_16 (60) = happyShift action_33
action_16 (61) = happyShift action_34
action_16 (86) = happyShift action_22
action_16 (88) = happyShift action_35
action_16 (24) = happyGoto action_23
action_16 (26) = happyGoto action_24
action_16 (41) = happyGoto action_25
action_16 (42) = happyGoto action_26
action_16 (43) = happyGoto action_41
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (48) = happyShift action_31
action_17 (59) = happyShift action_32
action_17 (60) = happyShift action_33
action_17 (61) = happyShift action_34
action_17 (86) = happyShift action_22
action_17 (88) = happyShift action_35
action_17 (24) = happyGoto action_23
action_17 (26) = happyGoto action_24
action_17 (41) = happyGoto action_25
action_17 (42) = happyGoto action_26
action_17 (43) = happyGoto action_27
action_17 (44) = happyGoto action_40
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (48) = happyShift action_31
action_18 (59) = happyShift action_32
action_18 (60) = happyShift action_33
action_18 (61) = happyShift action_34
action_18 (86) = happyShift action_22
action_18 (88) = happyShift action_35
action_18 (24) = happyGoto action_23
action_18 (26) = happyGoto action_24
action_18 (41) = happyGoto action_25
action_18 (42) = happyGoto action_26
action_18 (43) = happyGoto action_27
action_18 (44) = happyGoto action_28
action_18 (45) = happyGoto action_39
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (48) = happyShift action_31
action_19 (59) = happyShift action_32
action_19 (60) = happyShift action_33
action_19 (61) = happyShift action_34
action_19 (69) = happyShift action_38
action_19 (86) = happyShift action_22
action_19 (88) = happyShift action_35
action_19 (24) = happyGoto action_23
action_19 (26) = happyGoto action_24
action_19 (41) = happyGoto action_25
action_19 (42) = happyGoto action_26
action_19 (43) = happyGoto action_27
action_19 (44) = happyGoto action_28
action_19 (45) = happyGoto action_36
action_19 (46) = happyGoto action_37
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (48) = happyShift action_31
action_20 (59) = happyShift action_32
action_20 (60) = happyShift action_33
action_20 (61) = happyShift action_34
action_20 (86) = happyShift action_22
action_20 (88) = happyShift action_35
action_20 (24) = happyGoto action_23
action_20 (26) = happyGoto action_24
action_20 (41) = happyGoto action_25
action_20 (42) = happyGoto action_26
action_20 (43) = happyGoto action_27
action_20 (44) = happyGoto action_28
action_20 (45) = happyGoto action_29
action_20 (47) = happyGoto action_30
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (86) = happyShift action_22
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_21

action_23 _ = happyReduce_71

action_24 _ = happyReduce_68

action_25 _ = happyReduce_74

action_26 (50) = happyShift action_125
action_26 _ = happyReduce_76

action_27 (51) = happyShift action_124
action_27 _ = happyReduce_78

action_28 (54) = happyShift action_123
action_28 _ = happyReduce_80

action_29 _ = happyReduce_83

action_30 (89) = happyAccept
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (48) = happyShift action_31
action_31 (59) = happyShift action_32
action_31 (60) = happyShift action_33
action_31 (61) = happyShift action_34
action_31 (69) = happyShift action_38
action_31 (86) = happyShift action_22
action_31 (88) = happyShift action_35
action_31 (24) = happyGoto action_23
action_31 (26) = happyGoto action_24
action_31 (41) = happyGoto action_25
action_31 (42) = happyGoto action_26
action_31 (43) = happyGoto action_27
action_31 (44) = happyGoto action_28
action_31 (45) = happyGoto action_36
action_31 (46) = happyGoto action_122
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_70

action_33 (48) = happyShift action_31
action_33 (59) = happyShift action_32
action_33 (61) = happyShift action_34
action_33 (86) = happyShift action_22
action_33 (88) = happyShift action_35
action_33 (24) = happyGoto action_23
action_33 (26) = happyGoto action_24
action_33 (41) = happyGoto action_121
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_69

action_35 _ = happyReduce_23

action_36 _ = happyReduce_82

action_37 (89) = happyAccept
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (86) = happyShift action_22
action_38 (24) = happyGoto action_44
action_38 (40) = happyGoto action_120
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (89) = happyAccept
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (89) = happyAccept
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (89) = happyAccept
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (89) = happyAccept
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (89) = happyAccept
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_67

action_45 (89) = happyAccept
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_34

action_47 _ = happyReduce_37

action_48 _ = happyReduce_47

action_49 (48) = happyShift action_55
action_49 (62) = happyShift action_56
action_49 (67) = happyShift action_58
action_49 (81) = happyShift action_68
action_49 (86) = happyShift action_22
action_49 (87) = happyShift action_70
action_49 (24) = happyGoto action_46
action_49 (25) = happyGoto action_47
action_49 (30) = happyGoto action_94
action_49 _ = happyReduce_50

action_50 (51) = happyShift action_95
action_50 (53) = happyShift action_96
action_50 (57) = happyShift action_119
action_50 _ = happyReduce_52

action_51 _ = happyReduce_62

action_52 _ = happyReduce_66

action_53 _ = happyReduce_59

action_54 (89) = happyAccept
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (48) = happyShift action_55
action_55 (62) = happyShift action_56
action_55 (65) = happyShift action_57
action_55 (67) = happyShift action_58
action_55 (68) = happyShift action_59
action_55 (70) = happyShift action_60
action_55 (71) = happyShift action_61
action_55 (73) = happyShift action_62
action_55 (74) = happyShift action_63
action_55 (75) = happyShift action_64
action_55 (76) = happyShift action_65
action_55 (77) = happyShift action_66
action_55 (79) = happyShift action_67
action_55 (81) = happyShift action_68
action_55 (85) = happyShift action_69
action_55 (86) = happyShift action_22
action_55 (87) = happyShift action_70
action_55 (24) = happyGoto action_46
action_55 (25) = happyGoto action_47
action_55 (30) = happyGoto action_48
action_55 (31) = happyGoto action_49
action_55 (32) = happyGoto action_50
action_55 (33) = happyGoto action_51
action_55 (34) = happyGoto action_117
action_55 (35) = happyGoto action_118
action_55 (36) = happyGoto action_53
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (63) = happyShift action_116
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (48) = happyShift action_55
action_57 (62) = happyShift action_56
action_57 (65) = happyShift action_57
action_57 (67) = happyShift action_58
action_57 (68) = happyShift action_59
action_57 (70) = happyShift action_60
action_57 (71) = happyShift action_61
action_57 (73) = happyShift action_62
action_57 (74) = happyShift action_63
action_57 (75) = happyShift action_64
action_57 (76) = happyShift action_65
action_57 (77) = happyShift action_66
action_57 (79) = happyShift action_67
action_57 (81) = happyShift action_68
action_57 (85) = happyShift action_69
action_57 (86) = happyShift action_22
action_57 (87) = happyShift action_70
action_57 (24) = happyGoto action_46
action_57 (25) = happyGoto action_47
action_57 (30) = happyGoto action_48
action_57 (31) = happyGoto action_49
action_57 (32) = happyGoto action_50
action_57 (33) = happyGoto action_51
action_57 (34) = happyGoto action_115
action_57 (36) = happyGoto action_53
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_36

action_59 (48) = happyShift action_77
action_59 (62) = happyShift action_78
action_59 (64) = happyShift action_79
action_59 (73) = happyShift action_80
action_59 (74) = happyShift action_81
action_59 (86) = happyShift action_22
action_59 (24) = happyGoto action_71
action_59 (27) = happyGoto action_72
action_59 (28) = happyGoto action_73
action_59 (29) = happyGoto action_114
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (48) = happyShift action_55
action_60 (62) = happyShift action_56
action_60 (67) = happyShift action_58
action_60 (81) = happyShift action_68
action_60 (86) = happyShift action_22
action_60 (87) = happyShift action_70
action_60 (24) = happyGoto action_46
action_60 (25) = happyGoto action_47
action_60 (30) = happyGoto action_113
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (48) = happyShift action_55
action_61 (62) = happyShift action_56
action_61 (65) = happyShift action_57
action_61 (67) = happyShift action_58
action_61 (68) = happyShift action_59
action_61 (70) = happyShift action_60
action_61 (71) = happyShift action_61
action_61 (73) = happyShift action_62
action_61 (74) = happyShift action_63
action_61 (75) = happyShift action_64
action_61 (76) = happyShift action_65
action_61 (77) = happyShift action_66
action_61 (79) = happyShift action_67
action_61 (81) = happyShift action_68
action_61 (85) = happyShift action_69
action_61 (86) = happyShift action_22
action_61 (87) = happyShift action_70
action_61 (24) = happyGoto action_46
action_61 (25) = happyGoto action_47
action_61 (30) = happyGoto action_48
action_61 (31) = happyGoto action_49
action_61 (32) = happyGoto action_50
action_61 (33) = happyGoto action_51
action_61 (34) = happyGoto action_112
action_61 (36) = happyGoto action_53
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (48) = happyShift action_55
action_62 (62) = happyShift action_56
action_62 (67) = happyShift action_58
action_62 (81) = happyShift action_68
action_62 (86) = happyShift action_22
action_62 (87) = happyShift action_70
action_62 (24) = happyGoto action_46
action_62 (25) = happyGoto action_47
action_62 (30) = happyGoto action_111
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (48) = happyShift action_55
action_63 (62) = happyShift action_56
action_63 (67) = happyShift action_58
action_63 (81) = happyShift action_68
action_63 (86) = happyShift action_22
action_63 (87) = happyShift action_70
action_63 (24) = happyGoto action_46
action_63 (25) = happyGoto action_47
action_63 (30) = happyGoto action_110
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (48) = happyShift action_55
action_64 (62) = happyShift action_56
action_64 (67) = happyShift action_58
action_64 (81) = happyShift action_68
action_64 (86) = happyShift action_22
action_64 (87) = happyShift action_70
action_64 (24) = happyGoto action_46
action_64 (25) = happyGoto action_47
action_64 (30) = happyGoto action_109
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (48) = happyShift action_77
action_65 (62) = happyShift action_78
action_65 (64) = happyShift action_79
action_65 (73) = happyShift action_80
action_65 (74) = happyShift action_81
action_65 (86) = happyShift action_22
action_65 (24) = happyGoto action_71
action_65 (27) = happyGoto action_72
action_65 (28) = happyGoto action_73
action_65 (29) = happyGoto action_108
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (48) = happyShift action_77
action_66 (62) = happyShift action_78
action_66 (64) = happyShift action_79
action_66 (73) = happyShift action_80
action_66 (74) = happyShift action_81
action_66 (86) = happyShift action_22
action_66 (24) = happyGoto action_71
action_66 (27) = happyGoto action_72
action_66 (28) = happyGoto action_73
action_66 (29) = happyGoto action_107
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (48) = happyShift action_55
action_67 (62) = happyShift action_56
action_67 (67) = happyShift action_58
action_67 (81) = happyShift action_68
action_67 (86) = happyShift action_22
action_67 (87) = happyShift action_70
action_67 (24) = happyGoto action_46
action_67 (25) = happyGoto action_47
action_67 (30) = happyGoto action_106
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_35

action_69 (48) = happyShift action_77
action_69 (62) = happyShift action_78
action_69 (64) = happyShift action_79
action_69 (73) = happyShift action_80
action_69 (74) = happyShift action_81
action_69 (86) = happyShift action_22
action_69 (24) = happyGoto action_71
action_69 (27) = happyGoto action_72
action_69 (28) = happyGoto action_73
action_69 (29) = happyGoto action_105
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_22

action_71 _ = happyReduce_25

action_72 _ = happyReduce_31

action_73 (57) = happyShift action_104
action_73 _ = happyReduce_33

action_74 (54) = happyShift action_103
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (83) = happyShift action_102
action_75 _ = happyReduce_64

action_76 (89) = happyAccept
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (48) = happyShift action_77
action_77 (62) = happyShift action_78
action_77 (64) = happyShift action_79
action_77 (73) = happyShift action_80
action_77 (74) = happyShift action_81
action_77 (86) = happyShift action_22
action_77 (24) = happyGoto action_71
action_77 (27) = happyGoto action_72
action_77 (28) = happyGoto action_73
action_77 (29) = happyGoto action_101
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (63) = happyShift action_100
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_24

action_80 (48) = happyShift action_77
action_80 (62) = happyShift action_78
action_80 (64) = happyShift action_79
action_80 (86) = happyShift action_22
action_80 (24) = happyGoto action_71
action_80 (27) = happyGoto action_99
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (48) = happyShift action_77
action_81 (62) = happyShift action_78
action_81 (64) = happyShift action_79
action_81 (86) = happyShift action_22
action_81 (24) = happyGoto action_71
action_81 (27) = happyGoto action_98
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (89) = happyAccept
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (89) = happyAccept
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (56) = happyShift action_97
action_84 _ = happyReduce_61

action_85 (89) = happyAccept
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (89) = happyAccept
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (89) = happyAccept
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (51) = happyShift action_95
action_88 (53) = happyShift action_96
action_88 (89) = happyAccept
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (48) = happyShift action_55
action_89 (62) = happyShift action_56
action_89 (67) = happyShift action_58
action_89 (81) = happyShift action_68
action_89 (86) = happyShift action_22
action_89 (87) = happyShift action_70
action_89 (89) = happyAccept
action_89 (24) = happyGoto action_46
action_89 (25) = happyGoto action_47
action_89 (30) = happyGoto action_94
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (89) = happyAccept
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (89) = happyAccept
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (89) = happyAccept
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (89) = happyAccept
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_41

action_95 (48) = happyShift action_55
action_95 (62) = happyShift action_56
action_95 (67) = happyShift action_58
action_95 (70) = happyShift action_60
action_95 (73) = happyShift action_62
action_95 (74) = happyShift action_63
action_95 (75) = happyShift action_64
action_95 (79) = happyShift action_67
action_95 (81) = happyShift action_68
action_95 (86) = happyShift action_22
action_95 (87) = happyShift action_70
action_95 (24) = happyGoto action_46
action_95 (25) = happyGoto action_47
action_95 (30) = happyGoto action_48
action_95 (31) = happyGoto action_147
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (48) = happyShift action_55
action_96 (62) = happyShift action_56
action_96 (67) = happyShift action_58
action_96 (70) = happyShift action_60
action_96 (73) = happyShift action_62
action_96 (74) = happyShift action_63
action_96 (75) = happyShift action_64
action_96 (79) = happyShift action_67
action_96 (81) = happyShift action_68
action_96 (86) = happyShift action_22
action_96 (87) = happyShift action_70
action_96 (24) = happyGoto action_46
action_96 (25) = happyGoto action_47
action_96 (30) = happyGoto action_48
action_96 (31) = happyGoto action_146
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (48) = happyShift action_31
action_97 (59) = happyShift action_32
action_97 (60) = happyShift action_33
action_97 (61) = happyShift action_34
action_97 (69) = happyShift action_38
action_97 (86) = happyShift action_22
action_97 (88) = happyShift action_35
action_97 (24) = happyGoto action_23
action_97 (26) = happyGoto action_24
action_97 (41) = happyGoto action_25
action_97 (42) = happyGoto action_26
action_97 (43) = happyGoto action_27
action_97 (44) = happyGoto action_28
action_97 (45) = happyGoto action_36
action_97 (46) = happyGoto action_145
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_30

action_99 _ = happyReduce_29

action_100 _ = happyReduce_26

action_101 (49) = happyShift action_143
action_101 (52) = happyShift action_144
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (48) = happyShift action_77
action_102 (62) = happyShift action_78
action_102 (64) = happyShift action_79
action_102 (73) = happyShift action_80
action_102 (74) = happyShift action_81
action_102 (86) = happyShift action_22
action_102 (24) = happyGoto action_71
action_102 (27) = happyGoto action_72
action_102 (28) = happyGoto action_73
action_102 (29) = happyGoto action_74
action_102 (37) = happyGoto action_75
action_102 (38) = happyGoto action_142
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (48) = happyShift action_55
action_103 (62) = happyShift action_56
action_103 (65) = happyShift action_57
action_103 (67) = happyShift action_58
action_103 (68) = happyShift action_59
action_103 (70) = happyShift action_60
action_103 (71) = happyShift action_61
action_103 (73) = happyShift action_62
action_103 (74) = happyShift action_63
action_103 (75) = happyShift action_64
action_103 (76) = happyShift action_65
action_103 (77) = happyShift action_66
action_103 (79) = happyShift action_67
action_103 (81) = happyShift action_68
action_103 (85) = happyShift action_69
action_103 (86) = happyShift action_22
action_103 (87) = happyShift action_70
action_103 (24) = happyGoto action_46
action_103 (25) = happyGoto action_47
action_103 (30) = happyGoto action_48
action_103 (31) = happyGoto action_49
action_103 (32) = happyGoto action_50
action_103 (33) = happyGoto action_51
action_103 (34) = happyGoto action_52
action_103 (36) = happyGoto action_53
action_103 (39) = happyGoto action_141
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (48) = happyShift action_77
action_104 (62) = happyShift action_78
action_104 (64) = happyShift action_79
action_104 (73) = happyShift action_80
action_104 (74) = happyShift action_81
action_104 (86) = happyShift action_22
action_104 (24) = happyGoto action_71
action_104 (27) = happyGoto action_72
action_104 (28) = happyGoto action_73
action_104 (29) = happyGoto action_140
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (55) = happyShift action_139
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_43

action_107 (58) = happyShift action_138
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (58) = happyShift action_137
action_108 _ = happyFail (happyExpListPerState 108)

action_109 _ = happyReduce_46

action_110 _ = happyReduce_45

action_111 _ = happyReduce_44

action_112 (80) = happyShift action_136
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_42

action_114 (55) = happyShift action_135
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (78) = happyShift action_134
action_115 _ = happyFail (happyExpListPerState 115)

action_116 _ = happyReduce_38

action_117 (52) = happyShift action_133
action_117 (56) = happyShift action_97
action_117 _ = happyReduce_61

action_118 (49) = happyShift action_132
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (48) = happyShift action_55
action_119 (62) = happyShift action_56
action_119 (67) = happyShift action_58
action_119 (70) = happyShift action_60
action_119 (73) = happyShift action_62
action_119 (74) = happyShift action_63
action_119 (75) = happyShift action_64
action_119 (79) = happyShift action_67
action_119 (81) = happyShift action_68
action_119 (86) = happyShift action_22
action_119 (87) = happyShift action_70
action_119 (24) = happyGoto action_46
action_119 (25) = happyGoto action_47
action_119 (30) = happyGoto action_48
action_119 (31) = happyGoto action_49
action_119 (32) = happyGoto action_50
action_119 (33) = happyGoto action_131
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (55) = happyShift action_130
action_120 _ = happyFail (happyExpListPerState 120)

action_121 _ = happyReduce_73

action_122 (49) = happyShift action_129
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (48) = happyShift action_31
action_123 (59) = happyShift action_32
action_123 (60) = happyShift action_33
action_123 (61) = happyShift action_34
action_123 (86) = happyShift action_22
action_123 (88) = happyShift action_35
action_123 (24) = happyGoto action_23
action_123 (26) = happyGoto action_24
action_123 (41) = happyGoto action_25
action_123 (42) = happyGoto action_26
action_123 (43) = happyGoto action_27
action_123 (44) = happyGoto action_28
action_123 (45) = happyGoto action_128
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (48) = happyShift action_31
action_124 (59) = happyShift action_32
action_124 (60) = happyShift action_33
action_124 (61) = happyShift action_34
action_124 (86) = happyShift action_22
action_124 (88) = happyShift action_35
action_124 (24) = happyGoto action_23
action_124 (26) = happyGoto action_24
action_124 (41) = happyGoto action_25
action_124 (42) = happyGoto action_26
action_124 (43) = happyGoto action_27
action_124 (44) = happyGoto action_127
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (48) = happyShift action_31
action_125 (59) = happyShift action_32
action_125 (60) = happyShift action_33
action_125 (61) = happyShift action_34
action_125 (86) = happyShift action_22
action_125 (88) = happyShift action_35
action_125 (24) = happyGoto action_23
action_125 (26) = happyGoto action_24
action_125 (41) = happyGoto action_25
action_125 (42) = happyGoto action_26
action_125 (43) = happyGoto action_126
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_75

action_127 _ = happyReduce_77

action_128 _ = happyReduce_79

action_129 _ = happyReduce_72

action_130 (48) = happyShift action_31
action_130 (59) = happyShift action_32
action_130 (60) = happyShift action_33
action_130 (61) = happyShift action_34
action_130 (86) = happyShift action_22
action_130 (88) = happyShift action_35
action_130 (24) = happyGoto action_23
action_130 (26) = happyGoto action_24
action_130 (41) = happyGoto action_25
action_130 (42) = happyGoto action_26
action_130 (43) = happyGoto action_27
action_130 (44) = happyGoto action_28
action_130 (45) = happyGoto action_29
action_130 (47) = happyGoto action_156
action_130 _ = happyFail (happyExpListPerState 130)

action_131 _ = happyReduce_51

action_132 _ = happyReduce_40

action_133 (48) = happyShift action_55
action_133 (62) = happyShift action_56
action_133 (65) = happyShift action_57
action_133 (67) = happyShift action_58
action_133 (68) = happyShift action_59
action_133 (70) = happyShift action_60
action_133 (71) = happyShift action_61
action_133 (73) = happyShift action_62
action_133 (74) = happyShift action_63
action_133 (75) = happyShift action_64
action_133 (76) = happyShift action_65
action_133 (77) = happyShift action_66
action_133 (79) = happyShift action_67
action_133 (81) = happyShift action_68
action_133 (85) = happyShift action_69
action_133 (86) = happyShift action_22
action_133 (87) = happyShift action_70
action_133 (24) = happyGoto action_46
action_133 (25) = happyGoto action_47
action_133 (30) = happyGoto action_48
action_133 (31) = happyGoto action_49
action_133 (32) = happyGoto action_50
action_133 (33) = happyGoto action_51
action_133 (34) = happyGoto action_155
action_133 (36) = happyGoto action_53
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (82) = happyShift action_154
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (48) = happyShift action_55
action_135 (62) = happyShift action_56
action_135 (65) = happyShift action_57
action_135 (67) = happyShift action_58
action_135 (68) = happyShift action_59
action_135 (70) = happyShift action_60
action_135 (71) = happyShift action_61
action_135 (73) = happyShift action_62
action_135 (74) = happyShift action_63
action_135 (75) = happyShift action_64
action_135 (76) = happyShift action_65
action_135 (77) = happyShift action_66
action_135 (79) = happyShift action_67
action_135 (81) = happyShift action_68
action_135 (85) = happyShift action_69
action_135 (86) = happyShift action_22
action_135 (87) = happyShift action_70
action_135 (24) = happyGoto action_46
action_135 (25) = happyGoto action_47
action_135 (30) = happyGoto action_48
action_135 (31) = happyGoto action_49
action_135 (32) = happyGoto action_50
action_135 (33) = happyGoto action_51
action_135 (34) = happyGoto action_52
action_135 (36) = happyGoto action_53
action_135 (39) = happyGoto action_153
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (48) = happyShift action_55
action_136 (62) = happyShift action_56
action_136 (65) = happyShift action_57
action_136 (67) = happyShift action_58
action_136 (68) = happyShift action_59
action_136 (70) = happyShift action_60
action_136 (71) = happyShift action_61
action_136 (73) = happyShift action_62
action_136 (74) = happyShift action_63
action_136 (75) = happyShift action_64
action_136 (76) = happyShift action_65
action_136 (77) = happyShift action_66
action_136 (79) = happyShift action_67
action_136 (81) = happyShift action_68
action_136 (85) = happyShift action_69
action_136 (86) = happyShift action_22
action_136 (87) = happyShift action_70
action_136 (24) = happyGoto action_46
action_136 (25) = happyGoto action_47
action_136 (30) = happyGoto action_48
action_136 (31) = happyGoto action_49
action_136 (32) = happyGoto action_50
action_136 (33) = happyGoto action_51
action_136 (34) = happyGoto action_152
action_136 (36) = happyGoto action_53
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (48) = happyShift action_55
action_137 (62) = happyShift action_56
action_137 (65) = happyShift action_57
action_137 (67) = happyShift action_58
action_137 (68) = happyShift action_59
action_137 (70) = happyShift action_60
action_137 (71) = happyShift action_61
action_137 (73) = happyShift action_62
action_137 (74) = happyShift action_63
action_137 (75) = happyShift action_64
action_137 (76) = happyShift action_65
action_137 (77) = happyShift action_66
action_137 (79) = happyShift action_67
action_137 (81) = happyShift action_68
action_137 (85) = happyShift action_69
action_137 (86) = happyShift action_22
action_137 (87) = happyShift action_70
action_137 (24) = happyGoto action_46
action_137 (25) = happyGoto action_47
action_137 (30) = happyGoto action_48
action_137 (31) = happyGoto action_49
action_137 (32) = happyGoto action_50
action_137 (33) = happyGoto action_51
action_137 (34) = happyGoto action_151
action_137 (36) = happyGoto action_53
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (48) = happyShift action_55
action_138 (62) = happyShift action_56
action_138 (65) = happyShift action_57
action_138 (67) = happyShift action_58
action_138 (68) = happyShift action_59
action_138 (70) = happyShift action_60
action_138 (71) = happyShift action_61
action_138 (73) = happyShift action_62
action_138 (74) = happyShift action_63
action_138 (75) = happyShift action_64
action_138 (76) = happyShift action_65
action_138 (77) = happyShift action_66
action_138 (79) = happyShift action_67
action_138 (81) = happyShift action_68
action_138 (85) = happyShift action_69
action_138 (86) = happyShift action_22
action_138 (87) = happyShift action_70
action_138 (24) = happyGoto action_46
action_138 (25) = happyGoto action_47
action_138 (30) = happyGoto action_48
action_138 (31) = happyGoto action_49
action_138 (32) = happyGoto action_50
action_138 (33) = happyGoto action_51
action_138 (34) = happyGoto action_52
action_138 (36) = happyGoto action_53
action_138 (39) = happyGoto action_150
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (48) = happyShift action_55
action_139 (62) = happyShift action_56
action_139 (65) = happyShift action_57
action_139 (67) = happyShift action_58
action_139 (68) = happyShift action_59
action_139 (70) = happyShift action_60
action_139 (71) = happyShift action_61
action_139 (73) = happyShift action_62
action_139 (74) = happyShift action_63
action_139 (75) = happyShift action_64
action_139 (76) = happyShift action_65
action_139 (77) = happyShift action_66
action_139 (79) = happyShift action_67
action_139 (81) = happyShift action_68
action_139 (85) = happyShift action_69
action_139 (86) = happyShift action_22
action_139 (87) = happyShift action_70
action_139 (24) = happyGoto action_46
action_139 (25) = happyGoto action_47
action_139 (30) = happyGoto action_48
action_139 (31) = happyGoto action_49
action_139 (32) = happyGoto action_50
action_139 (33) = happyGoto action_51
action_139 (34) = happyGoto action_52
action_139 (36) = happyGoto action_53
action_139 (39) = happyGoto action_149
action_139 _ = happyFail (happyExpListPerState 139)

action_140 _ = happyReduce_32

action_141 _ = happyReduce_63

action_142 _ = happyReduce_65

action_143 _ = happyReduce_28

action_144 (48) = happyShift action_77
action_144 (62) = happyShift action_78
action_144 (64) = happyShift action_79
action_144 (73) = happyShift action_80
action_144 (74) = happyShift action_81
action_144 (86) = happyShift action_22
action_144 (24) = happyGoto action_71
action_144 (27) = happyGoto action_72
action_144 (28) = happyGoto action_73
action_144 (29) = happyGoto action_148
action_144 _ = happyFail (happyExpListPerState 144)

action_145 _ = happyReduce_60

action_146 (48) = happyShift action_55
action_146 (62) = happyShift action_56
action_146 (67) = happyShift action_58
action_146 (81) = happyShift action_68
action_146 (86) = happyShift action_22
action_146 (87) = happyShift action_70
action_146 (24) = happyGoto action_46
action_146 (25) = happyGoto action_47
action_146 (30) = happyGoto action_94
action_146 _ = happyReduce_49

action_147 (48) = happyShift action_55
action_147 (62) = happyShift action_56
action_147 (67) = happyShift action_58
action_147 (81) = happyShift action_68
action_147 (86) = happyShift action_22
action_147 (87) = happyShift action_70
action_147 (24) = happyGoto action_46
action_147 (25) = happyGoto action_47
action_147 (30) = happyGoto action_94
action_147 _ = happyReduce_48

action_148 (49) = happyShift action_162
action_148 _ = happyFail (happyExpListPerState 148)

action_149 _ = happyReduce_56

action_150 (72) = happyShift action_161
action_150 _ = happyFail (happyExpListPerState 150)

action_151 (72) = happyShift action_160
action_151 _ = happyFail (happyExpListPerState 151)

action_152 (66) = happyShift action_159
action_152 _ = happyFail (happyExpListPerState 152)

action_153 _ = happyReduce_57

action_154 (48) = happyShift action_77
action_154 (62) = happyShift action_78
action_154 (64) = happyShift action_79
action_154 (73) = happyShift action_80
action_154 (74) = happyShift action_81
action_154 (86) = happyShift action_22
action_154 (24) = happyGoto action_71
action_154 (27) = happyGoto action_72
action_154 (28) = happyGoto action_73
action_154 (29) = happyGoto action_74
action_154 (37) = happyGoto action_75
action_154 (38) = happyGoto action_158
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (49) = happyShift action_157
action_155 _ = happyFail (happyExpListPerState 155)

action_156 _ = happyReduce_81

action_157 _ = happyReduce_39

action_158 (84) = happyShift action_166
action_158 _ = happyFail (happyExpListPerState 158)

action_159 (48) = happyShift action_55
action_159 (62) = happyShift action_56
action_159 (65) = happyShift action_57
action_159 (67) = happyShift action_58
action_159 (68) = happyShift action_59
action_159 (70) = happyShift action_60
action_159 (71) = happyShift action_61
action_159 (73) = happyShift action_62
action_159 (74) = happyShift action_63
action_159 (75) = happyShift action_64
action_159 (76) = happyShift action_65
action_159 (77) = happyShift action_66
action_159 (79) = happyShift action_67
action_159 (81) = happyShift action_68
action_159 (85) = happyShift action_69
action_159 (86) = happyShift action_22
action_159 (87) = happyShift action_70
action_159 (24) = happyGoto action_46
action_159 (25) = happyGoto action_47
action_159 (30) = happyGoto action_48
action_159 (31) = happyGoto action_49
action_159 (32) = happyGoto action_50
action_159 (33) = happyGoto action_51
action_159 (34) = happyGoto action_165
action_159 (36) = happyGoto action_53
action_159 _ = happyFail (happyExpListPerState 159)

action_160 (48) = happyShift action_55
action_160 (62) = happyShift action_56
action_160 (65) = happyShift action_57
action_160 (67) = happyShift action_58
action_160 (68) = happyShift action_59
action_160 (70) = happyShift action_60
action_160 (71) = happyShift action_61
action_160 (73) = happyShift action_62
action_160 (74) = happyShift action_63
action_160 (75) = happyShift action_64
action_160 (76) = happyShift action_65
action_160 (77) = happyShift action_66
action_160 (79) = happyShift action_67
action_160 (81) = happyShift action_68
action_160 (85) = happyShift action_69
action_160 (86) = happyShift action_22
action_160 (87) = happyShift action_70
action_160 (24) = happyGoto action_46
action_160 (25) = happyGoto action_47
action_160 (30) = happyGoto action_48
action_160 (31) = happyGoto action_49
action_160 (32) = happyGoto action_50
action_160 (33) = happyGoto action_51
action_160 (34) = happyGoto action_52
action_160 (36) = happyGoto action_53
action_160 (39) = happyGoto action_164
action_160 _ = happyFail (happyExpListPerState 160)

action_161 (48) = happyShift action_55
action_161 (62) = happyShift action_56
action_161 (65) = happyShift action_57
action_161 (67) = happyShift action_58
action_161 (68) = happyShift action_59
action_161 (70) = happyShift action_60
action_161 (71) = happyShift action_61
action_161 (73) = happyShift action_62
action_161 (74) = happyShift action_63
action_161 (75) = happyShift action_64
action_161 (76) = happyShift action_65
action_161 (77) = happyShift action_66
action_161 (79) = happyShift action_67
action_161 (81) = happyShift action_68
action_161 (85) = happyShift action_69
action_161 (86) = happyShift action_22
action_161 (87) = happyShift action_70
action_161 (24) = happyGoto action_46
action_161 (25) = happyGoto action_47
action_161 (30) = happyGoto action_48
action_161 (31) = happyGoto action_49
action_161 (32) = happyGoto action_50
action_161 (33) = happyGoto action_51
action_161 (34) = happyGoto action_52
action_161 (36) = happyGoto action_53
action_161 (39) = happyGoto action_163
action_161 _ = happyFail (happyExpListPerState 161)

action_162 _ = happyReduce_27

action_163 _ = happyReduce_55

action_164 _ = happyReduce_54

action_165 _ = happyReduce_53

action_166 _ = happyReduce_58

happyReduce_21 = happySpecReduce_1  24 happyReduction_21
happyReduction_21 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn24
		 (FreeFoilTypecheck.MiniML.Parser.Abs.Ident happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  25 happyReduction_22
happyReduction_22 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn25
		 ((read happy_var_1) :: Integer
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  26 happyReduction_23
happyReduction_23 (HappyTerminal (PT _ (T_UVarIdent happy_var_1)))
	 =  HappyAbsSyn26
		 (FreeFoilTypecheck.MiniML.Parser.Abs.UVarIdent happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  27 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn27
		 (FreeFoilTypecheck.MiniML.Parser.Abs.PatternWildcard
	)

happyReduce_25 = happySpecReduce_1  27 happyReduction_25
happyReduction_25 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn27
		 (FreeFoilTypecheck.MiniML.Parser.Abs.PatternVar happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  27 happyReduction_26
happyReduction_26 _
	_
	 =  HappyAbsSyn27
		 (FreeFoilTypecheck.MiniML.Parser.Abs.PatternNil
	)

happyReduce_27 = happyReduce 5 27 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (FreeFoilTypecheck.MiniML.Parser.Abs.PatternPair happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_3  27 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  28 happyReduction_29
happyReduction_29 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (FreeFoilTypecheck.MiniML.Parser.Abs.PatternInl happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  28 happyReduction_30
happyReduction_30 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (FreeFoilTypecheck.MiniML.Parser.Abs.PatternInr happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  28 happyReduction_31
happyReduction_31 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  29 happyReduction_32
happyReduction_32 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (FreeFoilTypecheck.MiniML.Parser.Abs.PatternCons happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  29 happyReduction_33
happyReduction_33 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  30 happyReduction_34
happyReduction_34 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.EVar happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  30 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.ETrue
	)

happyReduce_36 = happySpecReduce_1  30 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.EFalse
	)

happyReduce_37 = happySpecReduce_1  30 happyReduction_37
happyReduction_37 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.ENat happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  30 happyReduction_38
happyReduction_38 _
	_
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.ENil
	)

happyReduce_39 = happyReduce 5 30 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.EPair happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_3  30 happyReduction_40
happyReduction_40 _
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (happy_var_2
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  31 happyReduction_41
happyReduction_41 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.EApp happy_var_1 happy_var_2
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  31 happyReduction_42
happyReduction_42 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.EFst happy_var_2
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  31 happyReduction_43
happyReduction_43 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.ESnd happy_var_2
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  31 happyReduction_44
happyReduction_44 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.EInl happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  31 happyReduction_45
happyReduction_45 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.EInr happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  31 happyReduction_46
happyReduction_46 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.EIsZero happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  31 happyReduction_47
happyReduction_47 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  32 happyReduction_48
happyReduction_48 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.EAdd happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  32 happyReduction_49
happyReduction_49 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.ESub happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  32 happyReduction_50
happyReduction_50 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  33 happyReduction_51
happyReduction_51 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.ECons happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  33 happyReduction_52
happyReduction_52 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happyReduce 6 34 happyReduction_53
happyReduction_53 ((HappyAbsSyn30  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.EIf happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_54 = happyReduce 6 34 happyReduction_54
happyReduction_54 ((HappyAbsSyn39  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.ELet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_55 = happyReduce 6 34 happyReduction_55
happyReduction_55 ((HappyAbsSyn39  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.ELetRec happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_56 = happyReduce 4 34 happyReduction_56
happyReduction_56 ((HappyAbsSyn39  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.EAbs happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_57 = happyReduce 4 34 happyReduction_57
happyReduction_57 ((HappyAbsSyn39  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.EFix happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_58 = happyReduce 6 34 happyReduction_58
happyReduction_58 (_ `HappyStk`
	(HappyAbsSyn38  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.ECase happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_59 = happySpecReduce_1  34 happyReduction_59
happyReduction_59 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  35 happyReduction_60
happyReduction_60 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (FreeFoilTypecheck.MiniML.Parser.Abs.ETyped happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  35 happyReduction_61
happyReduction_61 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  36 happyReduction_62
happyReduction_62 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  37 happyReduction_63
happyReduction_63 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn37
		 (FreeFoilTypecheck.MiniML.Parser.Abs.Branch happy_var_1 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  38 happyReduction_64
happyReduction_64 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn38
		 ((:[]) happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  38 happyReduction_65
happyReduction_65 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn38
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  39 happyReduction_66
happyReduction_66 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn39
		 (FreeFoilTypecheck.MiniML.Parser.Abs.ScopedExp happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  40 happyReduction_67
happyReduction_67 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn40
		 (FreeFoilTypecheck.MiniML.Parser.Abs.TPatternVar happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  41 happyReduction_68
happyReduction_68 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn41
		 (FreeFoilTypecheck.MiniML.Parser.Abs.TUVar happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  41 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn41
		 (FreeFoilTypecheck.MiniML.Parser.Abs.TNat
	)

happyReduce_70 = happySpecReduce_1  41 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn41
		 (FreeFoilTypecheck.MiniML.Parser.Abs.TBool
	)

happyReduce_71 = happySpecReduce_1  41 happyReduction_71
happyReduction_71 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn41
		 (FreeFoilTypecheck.MiniML.Parser.Abs.TVar happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  41 happyReduction_72
happyReduction_72 _
	(HappyAbsSyn41  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (happy_var_2
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_2  42 happyReduction_73
happyReduction_73 (HappyAbsSyn41  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (FreeFoilTypecheck.MiniML.Parser.Abs.TList happy_var_2
	)
happyReduction_73 _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  42 happyReduction_74
happyReduction_74 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  43 happyReduction_75
happyReduction_75 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (FreeFoilTypecheck.MiniML.Parser.Abs.TProd happy_var_1 happy_var_3
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  43 happyReduction_76
happyReduction_76 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  44 happyReduction_77
happyReduction_77 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (FreeFoilTypecheck.MiniML.Parser.Abs.TSum happy_var_1 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  44 happyReduction_78
happyReduction_78 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  45 happyReduction_79
happyReduction_79 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (FreeFoilTypecheck.MiniML.Parser.Abs.TArrow happy_var_1 happy_var_3
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  45 happyReduction_80
happyReduction_80 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happyReduce 4 46 happyReduction_81
happyReduction_81 ((HappyAbsSyn47  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (FreeFoilTypecheck.MiniML.Parser.Abs.TForAll happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_82 = happySpecReduce_1  46 happyReduction_82
happyReduction_82 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  47 happyReduction_83
happyReduction_83 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn47
		 (FreeFoilTypecheck.MiniML.Parser.Abs.ScopedType happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 89 89 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 48;
	PT _ (TS _ 2) -> cont 49;
	PT _ (TS _ 3) -> cont 50;
	PT _ (TS _ 4) -> cont 51;
	PT _ (TS _ 5) -> cont 52;
	PT _ (TS _ 6) -> cont 53;
	PT _ (TS _ 7) -> cont 54;
	PT _ (TS _ 8) -> cont 55;
	PT _ (TS _ 9) -> cont 56;
	PT _ (TS _ 10) -> cont 57;
	PT _ (TS _ 11) -> cont 58;
	PT _ (TS _ 12) -> cont 59;
	PT _ (TS _ 13) -> cont 60;
	PT _ (TS _ 14) -> cont 61;
	PT _ (TS _ 15) -> cont 62;
	PT _ (TS _ 16) -> cont 63;
	PT _ (TS _ 17) -> cont 64;
	PT _ (TS _ 18) -> cont 65;
	PT _ (TS _ 19) -> cont 66;
	PT _ (TS _ 20) -> cont 67;
	PT _ (TS _ 21) -> cont 68;
	PT _ (TS _ 22) -> cont 69;
	PT _ (TS _ 23) -> cont 70;
	PT _ (TS _ 24) -> cont 71;
	PT _ (TS _ 25) -> cont 72;
	PT _ (TS _ 26) -> cont 73;
	PT _ (TS _ 27) -> cont 74;
	PT _ (TS _ 28) -> cont 75;
	PT _ (TS _ 29) -> cont 76;
	PT _ (TS _ 30) -> cont 77;
	PT _ (TS _ 31) -> cont 78;
	PT _ (TS _ 32) -> cont 79;
	PT _ (TS _ 33) -> cont 80;
	PT _ (TS _ 34) -> cont 81;
	PT _ (TS _ 35) -> cont 82;
	PT _ (TS _ 36) -> cont 83;
	PT _ (TS _ 37) -> cont 84;
	PT _ (TS _ 38) -> cont 85;
	PT _ (TV happy_dollar_dollar) -> cont 86;
	PT _ (TI happy_dollar_dollar) -> cont 87;
	PT _ (T_UVarIdent happy_dollar_dollar) -> cont 88;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 89 tk tks = happyError' (tks, explist)
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
pPattern2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pPattern1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pPattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pExp6 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pExp5 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pExp4 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pExp3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn30 z -> happyReturn z; _other -> notHappyAtAll })

pBranch tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn37 z -> happyReturn z; _other -> notHappyAtAll })

pListBranch tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn38 z -> happyReturn z; _other -> notHappyAtAll })

pScopedExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_12 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pTypePattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_13 tks) (\x -> case x of {HappyAbsSyn40 z -> happyReturn z; _other -> notHappyAtAll })

pType5 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_14 tks) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

pType4 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_15 tks) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

pType3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_16 tks) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

pType2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_17 tks) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

pType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_18 tks) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_19 tks) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

pScopedType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_20 tks) (\x -> case x of {HappyAbsSyn47 z -> happyReturn z; _other -> notHappyAtAll })

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
