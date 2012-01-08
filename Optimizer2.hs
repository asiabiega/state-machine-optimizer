module Optimizer2 where
import Prelude
import AST

optimizations2 = [(notAccessibleBranchRemoval, "not-accesible-branch-removal")
                 ,(sameArgBranchRemoval, "same-arg-branch-removal")
                 ,(trivialAndRemoval, "trivial-and-removal")]
-------------------------------------------------------------------------------------------------------------

--TODO? : negacja warunków po przejściu do następnego brancha
--TODO? : uwzględnienie CASE

--INACCESSIBLE BRANCH REMOVAL

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

naBranchRemoval (TmCase (TmVar x) arms def) assumptions = 
            let def_ = (naBranchRemoval def assumptions) in
            let arms_ = map (\(x,y) -> (x, naBranchRemoval y assumptions)) arms in
            TmCase (TmVar x) arms_ def_

naBranchRemoval t assuptions = t


contradicts c = any (contradictsCond c)

contradictsCond (TmEquals v k) cond = checkIfContradicts v k cond
contradictsCond (TmAnd conds) cond = any (\x -> contradictsCond x cond) conds
contradictsCond (TmOr conds) cond = all (\x -> contradictsCond x cond) conds

checkIfContradicts v k (TmEquals v2 k2) = v == v2 && k /= k2
checkIfContradicts v k (TmAnd conds) = any (checkIfContradicts v k) conds
checkIfContradicts v k (TmOr conds) = all (checkIfContradicts v k) conds

-------------------------------------------------------------------------------------------------------------

--SAME ARG BRANCH REMOVAL

sameArgBranchRemoval :: Character -> Character
sameArgBranchRemoval = map (\(st, t)-> (st, saBranchRemoval t))

saBranchRemoval (TmIf cond tt elifs tf) = let l = rmDuplicated $ (cond, tt):elifs in
    let tt_ = saBranchRemoval tt in
    let elifs_ = map (\(x,y) -> (x, saBranchRemoval y)) (tail l) in --FIXME will fail on if A then T elseif A then T else T
    let tf_ = (saBranchRemoval tf) in
    TmIf cond tt_ elifs_ tf_

saBranchRemoval (TmCase (TmVar x) arms def) = 
            let def_ = saBranchRemoval def in
            let arms_ = map (\(x,y) -> (x, saBranchRemoval y)) arms in
            TmCase (TmVar x) arms_ def_

saBranchRemoval t = t

rmDuplicated [] = []
rmDuplicated ((cond,term):xs) = (cond,term) : rmDuplicated (filter (\(a,b) -> not(a == cond)) xs)

-------------------------------------------------------------------------------------------------------------

--'TRIVIAL AND' REMOVAL

trivialAndRemoval :: Character -> Character
trivialAndRemoval = map (\(st, t)-> (st, trivialAndTermRemoval t))

trivialAndTermRemoval (TmIf cond tt elifs tf) = 
    let cond_ = trivialAndConditionRemoval cond in
    let tt_ = trivialAndTermRemoval tt in
    let elifs_ = map (\(x,y) -> (trivialAndConditionRemoval x, trivialAndTermRemoval y)) elifs in
    let tf_ = trivialAndTermRemoval tf in
    TmIf cond_ tt_ elifs_ tf_

trivialAndTermRemoval (TmCase (TmVar x) arms def) = 
    let arms_ = map (\(x,y) -> (x, trivialAndTermRemoval y)) arms in
    let def_ = trivialAndTermRemoval def in
    TmCase (TmVar x) arms_ def_

trivialAndTermRemoval t = t


trivialAndConditionRemoval (TmAnd tests) = 
    let tests_ = map trivialAndConditionRemoval tests in
    TmAnd $ rmDuplicatedCond tests_
trivialAndConditionRemoval (TmOr tests) = TmOr $ map trivialAndConditionRemoval tests
trivialAndConditionRemoval t = t

rmDuplicatedCond [] = []
rmDuplicatedCond (cond:xs) = cond : rmDuplicatedCond (filter (\x -> not(x == cond)) xs)
