module Optimizer2 where
import Prelude

import AST

-------------------------------------------------------------------------------------------------------------

--TODO? : negacja warunków po przejściu do następnego brancha
--TODO? : uwzględnienie CASE

notAccessibleBranchRemoval :: Character -> Character
notAccessibleBranchRemoval = map (\(st, t)-> (st, naBranchRemoval t []))

naBranchRemoval :: Term -> [Condition] -> Term
naBranchRemoval (TmIf cond tt elifs tf) assumptions = 
            if cond == TmFalse then
                naBranchRemoval tf assumptions
            else if cond == TmTrue then
                naBranchRemoval tt assumptions
            else if contradicts cond assumptions then
                case elifs of
                    [] -> (naBranchRemoval tf assumptions)
                    (c, t):xs -> naBranchRemoval (TmIf c t xs tf) assumptions
            else
                let tt_ = (naBranchRemoval tt (cond:assumptions)) in
                let elifs_ = map (\(x,y) -> (x, naBranchRemoval y (x:assumptions))) elifs in
                let tf_ = (naBranchRemoval tf assumptions) in
                TmIf cond tt_ elifs_ tf_

naBranchRemoval t assuptions = t

contradicts c = any (contradictsCond c)

contradictsCond (TmEquals v k) cond = checkIfContradicts v k cond
contradictsCond (TmAnd conds) cond = any (\x -> contradictsCond x cond) conds
contradictsCond (TmOr conds) cond = all (\x -> contradictsCond x cond) conds

checkIfContradicts v k (TmEquals v2 k2) = v == v2 && k /= k2
checkIfContradicts v k (TmAnd conds) = any (checkIfContradicts v k) conds
checkIfContradicts v k (TmOr conds) = all (checkIfContradicts v k) conds

-------------------------------------------------------------------------------------------------------------




