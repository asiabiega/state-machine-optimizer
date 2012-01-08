-- @author: Joanna Biega
-- optimization module (some of the optimization rules)

module Optimizer2 where
import Prelude
import AST

import Data.Function
import Data.List

optimizations2 :: [(Character -> Tagger Character, String)]
optimizations2 = [(return . notAccessibleBranchRemoval, "not-accesible-branch-removal")
                 ,(return . sameArgBranchRemoval, "same-arg-branch-removal")
                 ,(trivialAndRemoval, "trivial-and-removal")]

-------------------------------------------------------------------------------------------------------------

--INACCESSIBLE BRANCH REMOVAL
--deletes if/elseif nodes that are contradictory, i.e. would never be executed

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
                let clean_elifs = filter (\(x,y) -> x /= TmFalse) elifs in
                let elifs_ = map (\(x,y) -> (x, naBranchRemoval y (x:assumptions))) clean_elifs in
                let tf_ = (naBranchRemoval tf assumptions) in
                TmIf cond tt_ elifs_ tf_

naBranchRemoval (TmCase (TmVar x) arms def) assumptions = 
            let def_ = (naBranchRemoval def assumptions) in
            let arms_ = map (\(v,y) -> (v, naBranchRemoval y assumptions)) arms in
            TmCase (TmVar x) arms_ def_

naBranchRemoval t _ = t


contradicts c = any (contradictsCond c)

-- contradiction checking functions
contradictsCond (TmEquals v k _) cond = checkIfContradicts v k cond
contradictsCond (TmAnd conds _) cond = any (\x -> contradictsCond x cond) conds
contradictsCond (TmOr conds _) cond = all (\x -> contradictsCond x cond) conds

checkIfContradicts v k (TmEquals v2 k2 _) = v == v2 && k /= k2
checkIfContradicts v k (TmAnd conds _) = any (checkIfContradicts v k) conds
checkIfContradicts v k (TmOr conds _) = all (checkIfContradicts v k) conds

-------------------------------------------------------------------------------------------------------------

--SAME ARG BRANCH REMOVAL
-- deletes elseif nodes that are not reachable, because the same condition has been present in one of previous if/elseifs

sameArgBranchRemoval :: Character -> Character
sameArgBranchRemoval = map (\(st, t)-> (st, saBranchRemoval t))

saBranchRemoval (TmIf cond tt elifs tf) = let l = rmDuplicated $ (cond, tt):elifs in
    let tt_ = saBranchRemoval tt in
    let elifs_ = map (\(x,y) -> (x, saBranchRemoval y)) (tail l) in --FIXME will fail on if A then T elseif A then T else T
    let tf_ = (saBranchRemoval tf) in
    TmIf cond tt_ elifs_ tf_

saBranchRemoval (TmCase (TmVar x) arms def) = 
            let def_ = saBranchRemoval def in
            let arms_ = map (\(v,y) -> (v, saBranchRemoval y)) arms in
            TmCase (TmVar x) arms_ def_

saBranchRemoval t = t

rmDuplicated = nubBy (on (==) fst)

-------------------------------------------------------------------------------------------------------------

--'TRIVIAL AND' REMOVAL
-- deletes same arguments from AND chain

trivialAndRemoval :: Character -> Tagger Character
trivialAndRemoval = mapM (\(st, t)-> do { mt <- trivialAndTermRemoval t; return (st, mt)})

trivialAndTermRemoval :: Term -> Tagger Term
trivialAndTermRemoval (TmIf cond tt elifs tf) = do
    cond_ <- trivialAndConditionRemoval cond
    tt_ <- trivialAndTermRemoval tt
    elifs_ <- mapM (\(x,y) -> do { mx <- trivialAndConditionRemoval x; my <- trivialAndTermRemoval y; return (mx, my)}) elifs
    tf_ <- trivialAndTermRemoval tf
    return $ TmIf cond_ tt_ elifs_ tf_
trivialAndTermRemoval (TmCase (TmVar x) arms def) = do
    arms_ <- mapM (\(v,y) -> do { my <- trivialAndTermRemoval y; return (v, my)}) arms
    def_ <- trivialAndTermRemoval def
    return $ TmCase (TmVar x) arms_ def_
trivialAndTermRemoval t = return t

trivialAndConditionRemoval :: Condition -> Tagger Condition
trivialAndConditionRemoval (TmAnd tests _) = do
    tests_ <- mapM trivialAndConditionRemoval tests
    cachedCondition $ TmAnd (rmDuplicatedCond tests_) 0
trivialAndConditionRemoval (TmOr tests _) = mapM trivialAndConditionRemoval tests >>= (\tests_ -> return $ TmOr tests_ 0) >>= cachedCondition
trivialAndConditionRemoval t = return t

rmDuplicatedCond [] = []
rmDuplicatedCond (cond:xs) = cond : rmDuplicatedCond (filter (\x -> not(x == cond)) xs)
-------------------------------------------------------------------------------------------------------------
