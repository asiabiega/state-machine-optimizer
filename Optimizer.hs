module Optimizer where
import Control.Arrow
import Control.Monad.State

import AST

optimizations = [(contradictoryAndRemoval, "contradictory-and-removal"),
    (stateNumberWildcarder, "state-number-wildcarder")]

-- | Applies given optimizations, until a fixpoint is reached
--fixOptimizations opts 


-- | Contradictory and condition removal, it changes the whole condition to TmFalse, when TmAnd conditions are contradictory
contradictoryAndRemoval :: Character -> Character
contradictoryAndRemoval = map contradictoryAndRemovalRule where
    contradictoryAndRemovalRule :: Rule -> Rule
    contradictoryAndRemovalRule (stnums, t) = (stnums, contradictoryAndRemovalTerm t)

    contradictoryAndRemovalTerm :: Term -> Term
    contradictoryAndRemovalTerm (TmIf cnd t1 elseifs t2) = TmIf (contradictoryAndRemovalCond cnd) (contradictoryAndRemovalTerm t1)
        (map (contradictoryAndRemovalCond *** contradictoryAndRemovalTerm) elseifs) (contradictoryAndRemovalTerm t2)
    contradictoryAndRemovalTerm (TmCase var arms t) = TmCase var (map (second contradictoryAndRemovalTerm) arms) (contradictoryAndRemovalTerm t)
    contradictoryAndRemovalTerm a@(TmDecision _ _) = a

    contradictoryAndRemovalCond :: Condition -> Condition
    contradictoryAndRemovalCond (TmAnd cnds) = let newCnds = map contradictoryAndRemovalCond cnds in
        if evalState (cleanAnds newCnds) [] then TmAnd newCnds else TmFalse
    contradictoryAndRemovalCond (TmOr cnds) = TmOr $ map contradictoryAndRemovalCond cnds
    contradictoryAndRemovalCond a = a

    cleanAnds :: [Condition] -> State [(Variable, Integer)] Bool
    cleanAnds (TmTrue:cnds) = cleanAnds cnds
    cleanAnds (TmFalse:_) = return False
    cleanAnds (TmAnd cnds1:cnds2) = cleanAnds $ cnds1 ++ cnds2
    cleanAnds (TmOr _:cnds2) = cleanAnds cnds2 --TODO think what can we do about ORs
    cleanAnds (TmEquals v i:cnds) = do
        s <- get
        if contradictsSet s (v,i)
            then return False
            else modify ((v,i):) >> cleanAnds cnds
    cleanAnds [] = return True

    contradictsSet :: [(Variable, Integer)] -> (Variable, Integer) -> Bool
    contradictsSet set (v,i) = case lookup v set of
        Nothing -> False
        Just i2 -> i2 /= i

-- | State number wildcarder, it changes an explicit state in a TmDecision statement to a wildcard if able
stateNumberWildcarder :: Character -> Character
stateNumberWildcarder = map stateNumberWildcarderRule where
    stateNumberWildcarderRule :: Rule -> Rule
    stateNumberWildcarderRule a@(_:_:_,_) = a
    stateNumberWildcarderRule ([], _) = error "empty rule"
    stateNumberWildcarderRule ([n], t) = ([n], evalState (stateNumberWildcarderTerm t) n)

    stateNumberWildcarderTerm :: Term -> State Integer Term --TODO reader monad?
    stateNumberWildcarderTerm (TmIf cnd t1 elseifs t2) = do
        mt1 <- stateNumberWildcarderTerm t1
        mei <- mapM (\(cnd, t) -> do
            mt <- stateNumberWildcarderTerm t
            return (cnd, mt)) elseifs
        mt2 <- stateNumberWildcarderTerm t2
        return $ TmIf cnd mt1 mei mt2
    stateNumberWildcarderTerm (TmCase var arms t1) = do
        mas <- mapM (\(vs, t) -> do
            mt <- stateNumberWildcarderTerm t
            return (vs, mt)) arms
        mt1 <- stateNumberWildcarderTerm t1
        return $ TmCase var mas mt1
    stateNumberWildcarderTerm a@(TmDecision TmCurrent _) = return a
    stateNumberWildcarderTerm a@(TmDecision (TmState i) u) = gets $ \i2 -> if i == i2 then TmDecision TmCurrent u else a
