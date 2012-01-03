module MachineSize where
import Prelude
import Data.List(elem, minimum, maximum)
import AST

msize character = foldl (+) 0 (map (msizeTerm.snd) character)

msizeTerm (TmIf cond tt elifs tf) = let msize_elifs = 
                                        foldl (+) 0 (map (\(x,y) -> (msizeCondition x) + (msizeTerm y)) elifs)
                                    in (msizeCondition cond) + (msizeTerm tt) + (msize_elifs) + (msizeTerm tf)
msizeTerm (TmDecision TmCurrent _) = 3
msizeTerm (TmDecision _ _) = 4
msizeTerm (TmCase v arms def) = let msize_arms = 
                                        foldl (+) 0 (map (\(_,y) -> (msizeTerm y)) arms)
                                    in 10 + msize_arms + (mspan arms) + (msizeTerm def)


mspan arms = let all_labels = 
                foldl (++) [] (map (\x -> fst x) arms)
            in (maximum all_labels) - (minimum all_labels) + 1



msizeCondition (TmEquals _ _) = 6
msizeCondition (TmAnd tests) = foldl (+) 0 (map msizeCondition tests)
msizeCondition (TmOr tests) = foldl (+) 0 (map msizeCondition tests)
