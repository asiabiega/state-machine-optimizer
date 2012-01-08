{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import Control.Monad.State
import qualified Data.Map as Map
import AST
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.8

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (Character) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (Character)
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ([Rule]) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([Rule])
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Rule) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Rule)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (Term) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (Term)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ([(Condition, Term)]) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ([(Condition, Term)])
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ((Condition, Term)) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ((Condition, Term))
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([(ValueSet, Term)]) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ([(ValueSet, Term)])
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ((ValueSet, Term)) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ((ValueSet, Term))
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Condition) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Condition)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ([Condition]) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ([Condition])
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (Variable) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (Variable)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ([StateNumber]) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ([StateNumber])
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (StateNumber) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (StateNumber)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (NewState) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (NewState)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Utterance) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Utterance)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([Integer]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([Integer])
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (ValueSet) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (ValueSet)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x5f\x00\x5f\x00\x5e\x00\x53\x00\x5b\x00\x5a\x00\x57\x00\x59\x00\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x02\x00\x55\x00\xf9\xff\x54\x00\x00\x00\x51\x00\x48\x00\x4b\x00\x00\x00\x00\x00\x50\x00\x01\x00\x4f\x00\x4e\x00\x4d\x00\x4c\x00\x4a\x00\x00\x00\x36\x00\x49\x00\x47\x00\x45\x00\x26\x00\x46\x00\x00\x00\x44\x00\x43\x00\x41\x00\x40\x00\x3f\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x3d\x00\x3c\x00\x33\x00\x00\x00\x3b\x00\x00\x00\x3a\x00\x38\x00\x37\x00\x30\x00\x35\x00\x00\x00\x34\x00\x00\x00\x32\x00\x31\x00\x2f\x00\x29\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x2e\x00\x00\x00\x21\x00\x00\x00\x00\x00\x1f\x00\x13\x00\x2a\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x1e\x00\x20\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x25\x00\x00\x00\x12\x00\x1a\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x17\x00\x00\x00\x23\x00\x00\x00\x22\x00\x03\x00\xfd\xff\x00\x00\x14\x00\x00\x00\x00\x00\x09\x00\x00\x00\xf4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xfc\xff\x00\x00\x00\x00\xfc\xff\xea\xff\x00\x00\xea\xff\xe9\xff\xfd\xff\xfe\xff\xeb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\x00\x00\x00\x00\x00\x00\xe8\xff\xe7\xff\x00\x00\x00\x00\xed\xff\x00\x00\xed\xff\x00\x00\x00\x00\xe6\xff\x00\x00\xf3\xff\x00\x00\xf3\xff\x00\x00\x00\x00\xf9\xff\xf6\xff\xed\xff\x00\x00\x00\x00\x00\x00\xf0\xff\x00\x00\xef\xff\xee\xff\x00\x00\xf6\xff\x00\x00\xec\xff\x00\x00\xf4\xff\x00\x00\x00\x00\x00\x00\xe4\xff\x00\x00\xf7\xff\x00\x00\xf1\xff\x00\x00\x00\x00\x00\x00\xe4\xff\x00\x00\xf8\xff\xf2\xff\xe5\xff\xe3\xff\x00\x00\xfa\xff\xf5\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x08\x00\x01\x00\x0f\x00\x0b\x00\x08\x00\x04\x00\x05\x00\x06\x00\x04\x00\x05\x00\x0a\x00\x03\x00\x0c\x00\x08\x00\x09\x00\x04\x00\x05\x00\x0f\x00\x06\x00\x07\x00\x06\x00\x07\x00\x03\x00\x08\x00\x09\x00\x08\x00\x09\x00\x0b\x00\x0c\x00\x0b\x00\x0c\x00\x01\x00\x02\x00\x01\x00\x02\x00\x0a\x00\x03\x00\x03\x00\x10\x00\x03\x00\x0e\x00\x0a\x00\x0d\x00\x08\x00\x03\x00\x00\x00\x03\x00\x03\x00\x08\x00\x03\x00\x02\x00\x0e\x00\x03\x00\x02\x00\x02\x00\x08\x00\x02\x00\x07\x00\x03\x00\x02\x00\x02\x00\x02\x00\x09\x00\x03\x00\x03\x00\x03\x00\xff\xff\x03\x00\x02\x00\x02\x00\x02\x00\x08\x00\x03\x00\x03\x00\x02\x00\xff\xff\x03\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x09\x00\x0d\x00\x02\x00\x02\x00\xff\xff\x03\x00\x08\x00\x02\x00\x02\x00\xff\xff\x03\x00\x08\x00\x02\x00\x02\x00\x0f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x17\x00\x1b\x00\x45\x00\x18\x00\x3f\x00\x10\x00\x11\x00\x12\x00\x3b\x00\x31\x00\x1c\x00\x47\x00\x1d\x00\x28\x00\x2f\x00\x30\x00\x31\x00\x40\x00\x35\x00\x23\x00\x22\x00\x23\x00\x3e\x00\x28\x00\x29\x00\x28\x00\x2b\x00\x0c\x00\x08\x00\x07\x00\x08\x00\x0a\x00\x05\x00\x04\x00\x05\x00\x2a\x00\x42\x00\x37\x00\x38\x00\x1d\x00\x1e\x00\x13\x00\x15\x00\x18\x00\x0d\x00\x03\x00\x4a\x00\x45\x00\x42\x00\x47\x00\x0f\x00\x35\x00\x49\x00\x0f\x00\x1a\x00\x42\x00\x0f\x00\x3b\x00\x44\x00\x0f\x00\x3a\x00\x33\x00\x26\x00\x3d\x00\x3e\x00\x2d\x00\x00\x00\x2f\x00\x1a\x00\x33\x00\x25\x00\x2e\x00\x34\x00\x37\x00\x25\x00\x00\x00\x27\x00\x28\x00\x1a\x00\x15\x00\x1a\x00\x0f\x00\x22\x00\x20\x00\x21\x00\x15\x00\x1a\x00\x00\x00\x13\x00\x0a\x00\x0f\x00\x07\x00\x00\x00\x0c\x00\x0a\x00\x07\x00\x03\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 28) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28)
	]

happy_n_terms = 16 :: Int
happy_n_nonterms = 17 :: Int

happyReduce_1 = happySpecReduce_3  0# happyReduction_1
happyReduction_1 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (happy_var_2
	)}

happyReduce_2 = happySpecReduce_2  1# happyReduction_2
happyReduction_2 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_3 = happySpecReduce_0  1# happyReduction_3
happyReduction_3  =  happyIn5
		 ([]
	)

happyReduce_4 = happyReduce 4# 2# happyReduction_4
happyReduction_4 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut15 happy_x_2 of { happy_var_2 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 ((happy_var_2, happy_var_3)
	) `HappyStk` happyRest}}

happyReduce_5 = happyReduce 9# 3# happyReduction_5
happyReduction_5 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	case happyOut8 happy_x_6 of { happy_var_6 -> 
	case happyOut7 happy_x_8 of { happy_var_8 -> 
	happyIn7
		 (TmIf happy_var_3 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_6 = happyReduce 5# 3# happyReduction_6
happyReduction_6 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut17 happy_x_3 of { happy_var_3 -> 
	case happyOut18 happy_x_4 of { happy_var_4 -> 
	happyIn7
		 (TmDecision happy_var_3 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_7 = happyReduce 8# 3# happyReduction_7
happyReduction_7 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_3 of { happy_var_3 -> 
	case happyOut10 happy_x_5 of { happy_var_5 -> 
	case happyOut7 happy_x_7 of { happy_var_7 -> 
	happyIn7
		 (TmCase happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_8 = happySpecReduce_2  4# happyReduction_8
happyReduction_8 happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn8
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_9 = happySpecReduce_0  4# happyReduction_9
happyReduction_9  =  happyIn8
		 ([]
	)

happyReduce_10 = happyReduce 5# 5# happyReduction_10
happyReduction_10 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn9
		 ((happy_var_3, happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_11 = happySpecReduce_2  6# happyReduction_11
happyReduction_11 happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_12 = happySpecReduce_0  6# happyReduction_12
happyReduction_12  =  happyIn10
		 ([]
	)

happyReduce_13 = happyReduce 5# 7# happyReduction_13
happyReduction_13 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut20 happy_x_3 of { happy_var_3 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	happyIn11
		 ((happy_var_3, happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_14 = happyMonadReduce 5# 8# happyReduction_14
happyReduction_14 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut14 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { ((_,TkInt happy_var_4)) -> 
	( cachedCondition (TmEquals happy_var_3 happy_var_4 0))}}
	) (\r -> happyReturn (happyIn12 r))

happyReduce_15 = happyMonadReduce 4# 8# happyReduction_15
happyReduction_15 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut13 happy_x_3 of { happy_var_3 -> 
	( cachedCondition (TmAnd happy_var_3 0))}
	) (\r -> happyReturn (happyIn12 r))

happyReduce_16 = happyMonadReduce 4# 8# happyReduction_16
happyReduction_16 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut13 happy_x_3 of { happy_var_3 -> 
	( cachedCondition (TmOr happy_var_3 0))}
	) (\r -> happyReturn (happyIn12 r))

happyReduce_17 = happySpecReduce_2  9# happyReduction_17
happyReduction_17 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_18 = happySpecReduce_0  9# happyReduction_18
happyReduction_18  =  happyIn13
		 ([]
	)

happyReduce_19 = happyReduce 4# 10# happyReduction_19
happyReduction_19 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { ((_,TkString happy_var_3)) -> 
	happyIn14
		 (TmVar happy_var_3
	) `HappyStk` happyRest}

happyReduce_20 = happySpecReduce_2  11# happyReduction_20
happyReduction_20 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_21 = happySpecReduce_0  11# happyReduction_21
happyReduction_21  =  happyIn15
		 ([]
	)

happyReduce_22 = happySpecReduce_1  12# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOutTok happy_x_1 of { ((_,TkInt happy_var_1)) -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_23 = happySpecReduce_1  13# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOutTok happy_x_1 of { ((_,TkInt happy_var_1)) -> 
	happyIn17
		 (TmState happy_var_1
	)}

happyReduce_24 = happySpecReduce_1  13# happyReduction_24
happyReduction_24 happy_x_1
	 =  happyIn17
		 (TmCurrent
	)

happyReduce_25 = happySpecReduce_1  14# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOutTok happy_x_1 of { ((_,TkString happy_var_1)) -> 
	happyIn18
		 (happy_var_1
	)}

happyReduce_26 = happySpecReduce_2  15# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { ((_,TkInt happy_var_1)) -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_27 = happySpecReduce_0  15# happyReduction_27
happyReduction_27  =  happyIn19
		 ([]
	)

happyReduce_28 = happySpecReduce_3  16# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (happy_var_2
	)}

happyNewToken action sts stk [] =
	happyDoAction 15# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	(_,TkAnd) -> cont 1#;
	(_,TkLParen) -> cont 2#;
	(_,TkRParen) -> cont 3#;
	(_,TkIf) -> cont 4#;
	(_,TkDecision) -> cont 5#;
	(_,TkCase) -> cont 6#;
	(_,TkElseIf) -> cont 7#;
	(_,TkInt happy_dollar_dollar) -> cont 8#;
	(_,TkString happy_dollar_dollar) -> cont 9#;
	(_,TkEquals) -> cont 10#;
	(_,TkWildcard) -> cont 11#;
	(_,TkOr) -> cont 12#;
	(_,TkVar) -> cont 13#;
	(_,TkArm) -> cont 14#;
	_ -> happyError' (tk:tks)
	}

happyError_ 15# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => State (Int, Map.Map Condition Int) a -> (a -> State (Int, Map.Map Condition Int) b) -> State (Int, Map.Map Condition Int) b
happyThen = (>>=)
happyReturn :: () => a -> State (Int, Map.Map Condition Int) a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> State (Int, Map.Map Condition Int) a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> State (Int, Map.Map Condition Int) a
happyError' = parseError

parse tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut4 x))

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


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





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

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n Happy_GHC_Exts.<# (0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where (new_state) = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where (off)    = indexShortOffAddr happyActOffsets st
         (off_i)  = (off Happy_GHC_Exts.+# i)
	 check  = if (off_i Happy_GHC_Exts.>=# (0# :: Happy_GHC_Exts.Int#))
			then (indexShortOffAddr happyCheck off_i Happy_GHC_Exts.==#  i)
			else False
         (action)
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st

{-# LINE 130 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
	Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 163 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             (off) = indexShortOffAddr happyGotoOffsets st1
             (off_i) = (off Happy_GHC_Exts.+# nt)
             (new_state) = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where (off) = indexShortOffAddr happyGotoOffsets st
         (off_i) = (off Happy_GHC_Exts.+# nt)
         (new_state) = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

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


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
