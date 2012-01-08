-- @author: Joanna Biega
-- machine size measuring module

module MachineSize where
import Prelude hiding (lex)

import AST

msize :: Character -> Integer
msize character = sum (map (msizeTerm.snd) character)

msizeTerm :: Term -> Integer
msizeTerm (TmIf cond tt elifs tf) = let msize_elifs = sum (map (\(x,y) -> (msizeCondition x) + (msizeTerm y)) elifs)
    in (msizeCondition cond) + (msizeTerm tt) + (msize_elifs) + (msizeTerm tf)
msizeTerm (TmDecision TmCurrent _) = 3
msizeTerm (TmDecision _ _) = 4
msizeTerm (TmCase _ arms def) = let msize_arms = sum (map (\(_,y) -> (msizeTerm y)) arms)
    in 10 + msize_arms + (mspan arms) + (msizeTerm def)

mspan :: [(ValueSet, Term)] -> Integer
mspan arms = let all_labels = concat (map fst arms) in
    maximum all_labels - minimum all_labels + 1

msizeCondition :: Condition -> Integer
msizeCondition (TmEquals _ _ _) = 6
msizeCondition (TmAnd tests _) = sum (map msizeCondition tests)
msizeCondition (TmOr tests _) = sum (map msizeCondition tests)
msizeCondition TmFalse = 0
msizeCondition TmTrue = 0
