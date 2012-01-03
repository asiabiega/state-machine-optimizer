{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import AST
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Character)
	| HappyAbsSyn5 ([Rule])
	| HappyAbsSyn6 (Rule)
	| HappyAbsSyn7 (Term)
	| HappyAbsSyn8 ([(Condition, Term)])
	| HappyAbsSyn9 ((Condition, Term))
	| HappyAbsSyn10 ([(ValueSet, Term)])
	| HappyAbsSyn11 ((ValueSet, Term))
	| HappyAbsSyn12 (Condition)
	| HappyAbsSyn13 ([Condition])
	| HappyAbsSyn14 (Variable)
	| HappyAbsSyn15 ([StateNumber])
	| HappyAbsSyn16 (StateNumber)
	| HappyAbsSyn17 (NewState)
	| HappyAbsSyn18 (Utterance)
	| HappyAbsSyn19 ([Integer])
	| HappyAbsSyn20 (ValueSet)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Happy_GHC_Exts.Int# 
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
 action_73 :: () => Happy_GHC_Exts.Int# -> ({-HappyReduction (HappyIdentity) = -}
	   Happy_GHC_Exts.Int# 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
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
 happyReduce_28 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Happy_GHC_Exts.Int# 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (22#) = happyShift action_2
action_0 (4#) = happyGoto action_3
action_0 x = happyTcHack x happyFail

action_1 (22#) = happyShift action_2
action_1 x = happyTcHack x happyFail

action_2 (22#) = happyShift action_6
action_2 (5#) = happyGoto action_4
action_2 (6#) = happyGoto action_5
action_2 x = happyTcHack x happyReduce_3

action_3 (35#) = happyAccept
action_3 x = happyTcHack x happyFail

action_4 (23#) = happyShift action_11
action_4 x = happyTcHack x happyFail

action_5 (22#) = happyShift action_6
action_5 (5#) = happyGoto action_10
action_5 (6#) = happyGoto action_5
action_5 x = happyTcHack x happyReduce_3

action_6 (28#) = happyShift action_9
action_6 (15#) = happyGoto action_7
action_6 (16#) = happyGoto action_8
action_6 x = happyTcHack x happyReduce_21

action_7 (22#) = happyShift action_14
action_7 (7#) = happyGoto action_13
action_7 x = happyTcHack x happyFail

action_8 (28#) = happyShift action_9
action_8 (15#) = happyGoto action_12
action_8 (16#) = happyGoto action_8
action_8 x = happyTcHack x happyReduce_21

action_9 x = happyTcHack x happyReduce_22

action_10 x = happyTcHack x happyReduce_2

action_11 x = happyTcHack x happyReduce_1

action_12 x = happyTcHack x happyReduce_20

action_13 (23#) = happyShift action_18
action_13 x = happyTcHack x happyFail

action_14 (24#) = happyShift action_15
action_14 (25#) = happyShift action_16
action_14 (26#) = happyShift action_17
action_14 x = happyTcHack x happyFail

action_15 (22#) = happyShift action_25
action_15 (12#) = happyGoto action_24
action_15 x = happyTcHack x happyFail

action_16 (28#) = happyShift action_22
action_16 (31#) = happyShift action_23
action_16 (17#) = happyGoto action_21
action_16 x = happyTcHack x happyFail

action_17 (22#) = happyShift action_20
action_17 (14#) = happyGoto action_19
action_17 x = happyTcHack x happyFail

action_18 x = happyTcHack x happyReduce_4

action_19 (22#) = happyShift action_33
action_19 x = happyTcHack x happyFail

action_20 (33#) = happyShift action_32
action_20 x = happyTcHack x happyFail

action_21 (29#) = happyShift action_31
action_21 (18#) = happyGoto action_30
action_21 x = happyTcHack x happyFail

action_22 x = happyTcHack x happyReduce_23

action_23 x = happyTcHack x happyReduce_24

action_24 (22#) = happyShift action_14
action_24 (7#) = happyGoto action_29
action_24 x = happyTcHack x happyFail

action_25 (21#) = happyShift action_26
action_25 (30#) = happyShift action_27
action_25 (32#) = happyShift action_28
action_25 x = happyTcHack x happyFail

action_26 (22#) = happyShift action_25
action_26 (12#) = happyGoto action_40
action_26 (13#) = happyGoto action_43
action_26 x = happyTcHack x happyReduce_18

action_27 (22#) = happyShift action_20
action_27 (14#) = happyGoto action_42
action_27 x = happyTcHack x happyFail

action_28 (22#) = happyShift action_25
action_28 (12#) = happyGoto action_40
action_28 (13#) = happyGoto action_41
action_28 x = happyTcHack x happyReduce_18

action_29 (22#) = happyShift action_39
action_29 x = happyTcHack x happyFail

action_30 (23#) = happyShift action_38
action_30 x = happyTcHack x happyFail

action_31 x = happyTcHack x happyReduce_25

action_32 (29#) = happyShift action_37
action_32 x = happyTcHack x happyFail

action_33 (22#) = happyShift action_36
action_33 (10#) = happyGoto action_34
action_33 (11#) = happyGoto action_35
action_33 x = happyTcHack x happyReduce_12

action_34 (23#) = happyShift action_54
action_34 x = happyTcHack x happyFail

action_35 (22#) = happyShift action_36
action_35 (10#) = happyGoto action_53
action_35 (11#) = happyGoto action_35
action_35 x = happyTcHack x happyReduce_12

action_36 (34#) = happyShift action_52
action_36 x = happyTcHack x happyFail

action_37 (23#) = happyShift action_51
action_37 x = happyTcHack x happyFail

action_38 x = happyTcHack x happyReduce_6

action_39 (22#) = happyShift action_50
action_39 (8#) = happyGoto action_48
action_39 (9#) = happyGoto action_49
action_39 x = happyTcHack x happyReduce_9

action_40 (22#) = happyShift action_25
action_40 (12#) = happyGoto action_40
action_40 (13#) = happyGoto action_47
action_40 x = happyTcHack x happyReduce_18

action_41 (23#) = happyShift action_46
action_41 x = happyTcHack x happyFail

action_42 (28#) = happyShift action_45
action_42 x = happyTcHack x happyFail

action_43 (23#) = happyShift action_44
action_43 x = happyTcHack x happyFail

action_44 x = happyTcHack x happyReduce_15

action_45 (23#) = happyShift action_61
action_45 x = happyTcHack x happyFail

action_46 x = happyTcHack x happyReduce_16

action_47 x = happyTcHack x happyReduce_17

action_48 (23#) = happyShift action_60
action_48 x = happyTcHack x happyFail

action_49 (22#) = happyShift action_50
action_49 (8#) = happyGoto action_59
action_49 (9#) = happyGoto action_49
action_49 x = happyTcHack x happyReduce_9

action_50 (27#) = happyShift action_58
action_50 x = happyTcHack x happyFail

action_51 x = happyTcHack x happyReduce_19

action_52 (22#) = happyShift action_57
action_52 (20#) = happyGoto action_56
action_52 x = happyTcHack x happyFail

action_53 x = happyTcHack x happyReduce_11

action_54 (22#) = happyShift action_14
action_54 (7#) = happyGoto action_55
action_54 x = happyTcHack x happyFail

action_55 (23#) = happyShift action_67
action_55 x = happyTcHack x happyFail

action_56 (22#) = happyShift action_14
action_56 (7#) = happyGoto action_66
action_56 x = happyTcHack x happyFail

action_57 (28#) = happyShift action_65
action_57 (19#) = happyGoto action_64
action_57 x = happyTcHack x happyReduce_27

action_58 (22#) = happyShift action_25
action_58 (12#) = happyGoto action_63
action_58 x = happyTcHack x happyFail

action_59 x = happyTcHack x happyReduce_8

action_60 (22#) = happyShift action_14
action_60 (7#) = happyGoto action_62
action_60 x = happyTcHack x happyFail

action_61 x = happyTcHack x happyReduce_14

action_62 (23#) = happyShift action_72
action_62 x = happyTcHack x happyFail

action_63 (22#) = happyShift action_14
action_63 (7#) = happyGoto action_71
action_63 x = happyTcHack x happyFail

action_64 (23#) = happyShift action_70
action_64 x = happyTcHack x happyFail

action_65 (28#) = happyShift action_65
action_65 (19#) = happyGoto action_69
action_65 x = happyTcHack x happyReduce_27

action_66 (23#) = happyShift action_68
action_66 x = happyTcHack x happyFail

action_67 x = happyTcHack x happyReduce_7

action_68 x = happyTcHack x happyReduce_13

action_69 x = happyTcHack x happyReduce_26

action_70 x = happyTcHack x happyReduce_28

action_71 (23#) = happyShift action_73
action_71 x = happyTcHack x happyFail

action_72 x = happyTcHack x happyReduce_5

action_73 x = happyTcHack x happyReduce_10

happyReduce_1 = happySpecReduce_3  4# happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5# happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5# happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happyReduce 4# 6# happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_2, happy_var_3)
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 9# 7# happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TmIf happy_var_3 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 5# 7# happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TmDecision happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 8# 7# happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TmCase happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_2  8# happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  8# happyReduction_9
happyReduction_9  =  HappyAbsSyn8
		 ([]
	)

happyReduce_10 = happyReduce 5# 9# happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((happy_var_3, happy_var_4)
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_2  10# happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  10# happyReduction_12
happyReduction_12  =  HappyAbsSyn10
		 ([]
	)

happyReduce_13 = happyReduce 5# 11# happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((happy_var_3, happy_var_4)
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 5# 12# happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyTerminal ((_,TkInt happy_var_4))) `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TmEquals happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 4# 12# happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TmAnd happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4# 12# happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TmOr happy_var_3
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_2  13# happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  13# happyReduction_18
happyReduction_18  =  HappyAbsSyn13
		 ([]
	)

happyReduce_19 = happyReduce 4# 14# happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyTerminal ((_,TkString happy_var_3))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (TmVar happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_2  15# happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_0  15# happyReduction_21
happyReduction_21  =  HappyAbsSyn15
		 ([]
	)

happyReduce_22 = happySpecReduce_1  16# happyReduction_22
happyReduction_22 (HappyTerminal ((_,TkInt happy_var_1)))
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  17# happyReduction_23
happyReduction_23 (HappyTerminal ((_,TkInt happy_var_1)))
	 =  HappyAbsSyn17
		 (TmState happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  17# happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn17
		 (TmCurrent
	)

happyReduce_25 = happySpecReduce_1  18# happyReduction_25
happyReduction_25 (HappyTerminal ((_,TkString happy_var_1)))
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  19# happyReduction_26
happyReduction_26 (HappyAbsSyn19  happy_var_2)
	(HappyTerminal ((_,TkInt happy_var_1)))
	 =  HappyAbsSyn19
		 (happy_var_1 : happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  19# happyReduction_27
happyReduction_27  =  HappyAbsSyn19
		 ([]
	)

happyReduce_28 = happySpecReduce_3  20# happyReduction_28
happyReduction_28 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 35# 35# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	(_,TkAnd) -> cont 21#;
	(_,TkLParen) -> cont 22#;
	(_,TkRParen) -> cont 23#;
	(_,TkIf) -> cont 24#;
	(_,TkDecision) -> cont 25#;
	(_,TkCase) -> cont 26#;
	(_,TkElseIf) -> cont 27#;
	(_,TkInt happy_dollar_dollar) -> cont 28#;
	(_,TkString happy_dollar_dollar) -> cont 29#;
	(_,TkEquals) -> cont 30#;
	(_,TkWildcard) -> cont 31#;
	(_,TkOr) -> cont 32#;
	(_,TkVar) -> cont 33#;
	(_,TkArm) -> cont 34#;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError (((line,col),t):xs) = error $ "Parse error at token " ++ show t ++", line " ++ (show line) ++ ", column " ++ (show col)
parseError [] = error "Parse error at the end"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 1#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 1# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j ) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Happy_GHC_Exts.Int# ->                    -- token number
         Happy_GHC_Exts.Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (1# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  1# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  1# tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action 1# 1# tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action 1# 1# tk (HappyState (action)) sts ( (HappyErrorToken (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 311 "templates/GenericTemplate.hs" #-}
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
