module MachineSize where
import Prelude hiding (lex)
import System.Timeout
import System.Environment
import System.IO
import System.Exit
import Control.Concurrent.MVar

import Lexer
import Parser
import AST

msize character = sum (map (msizeTerm.snd) character)

msizeTerm (TmIf cond tt elifs tf) = let msize_elifs = sum (map (\(x,y) -> (msizeCondition x) + (msizeTerm y)) elifs)
    in (msizeCondition cond) + (msizeTerm tt) + (msize_elifs) + (msizeTerm tf)
msizeTerm (TmDecision TmCurrent _) = 3
msizeTerm (TmDecision _ _) = 4
msizeTerm (TmCase v arms def) = let msize_arms = sum (map (\(_,y) -> (msizeTerm y)) arms)
                                    in 10 + msize_arms + (mspan arms) + (msizeTerm def)

mspan arms = let all_labels = concat (map fst arms)
    in (maximum all_labels) - (minimum all_labels) + 1

msizeCondition (TmEquals _ _) = 6
msizeCondition (TmAnd tests) = sum (map msizeCondition tests)
msizeCondition (TmOr tests) = sum (map msizeCondition tests)
msizeCondition TmFalse = 0
msizeCondition TmTrue = 0
