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
-- Character-to-condition-for-each-decision helper function

termToConditionList :: Term -> [(Bool, Condition)] -> [([(Bool, Condition)], Term)]
termToConditionList (TmIf cond tt elifs tf) cnd = 
    let cond_tt = termToConditionList tt cnd in
    --(condition, list of condition-term pairs for t)
    let cond_elifs = map (\(c, t) -> (c, termToConditionList t cnd)) elifs in
    let cond_tf = termToConditionList tf cnd in
    --common list of(cond, condition-term pairs)
    let full_cond_list = (cond, cond_tt) : cond_elifs ++ [(TmTrue, cond_tf)] in
    -- map current condition (in split version) to list of needed conditions
    let full_cond_list_with_current = map (\(cond, tt) -> (splitCond cond, [ ( x ++ q,t) | x<-(splitCond cond), (q,t) <- tt ])) full_cond_list in
    let (negs, almost) = foldl (\(neg, curr) (c,t)-> (neg ++ (negateSplitCond c), [ ( x ++ q,tr) | x<-neg, (q,tr) <- t ] :curr)) ([],[]) full_cond_list_with_current in
    concat almost

termToConditionList (TmCase (TmVar x) arms def) cnd = 
    let cond_def = termToConditionList def cnd in
    let cond_arms = map (\(vals, t) -> (vals, termToConditionList t cnd)) arms in
    let cond_arms_ = concatMap (\(vals, cl) -> [ ( (True, TmEquals (TmVar x) p 0):q, t) | p <- vals, (q,t) <- cl ]) cond_arms in
    cond_def ++ cond_arms_

termToConditionList (TmDecision s u) cond = [(cond, TmDecision s u)]

--TODO: split cond
splitCond c = [[(True,c)]]

--TODO: negate
negateSplitCond c = undefined

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

contradictsCond (TmEquals v k _) cond = checkIfContradicts v k cond
contradictsCond (TmAnd conds _) cond = any (\x -> contradictsCond x cond) conds
contradictsCond (TmOr conds _) cond = all (\x -> contradictsCond x cond) conds

checkIfContradicts v k (TmEquals v2 k2 _) = v == v2 && k /= k2
checkIfContradicts v k (TmAnd conds _) = any (checkIfContradicts v k) conds
checkIfContradicts v k (TmOr conds _) = all (checkIfContradicts v k) conds

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

rmDuplicated = nubBy (on (==) fst)

-------------------------------------------------------------------------------------------------------------

--'TRIVIAL AND' REMOVAL

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
    arms_ <- mapM (\(x,y) -> do { my <- trivialAndTermRemoval y; return (x, my)}) arms
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
