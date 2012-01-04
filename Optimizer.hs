module Optimizer where
import Control.Arrow
import Control.Monad.State

import AST


--contradictory and condition removal, it changes the whole condition to TmFalse, when TmAnd conditions are contradictory
contradictoryAndRemoval :: Character -> Character
contradictoryAndRemoval = map contradictoryAndRemovalRule where
    contradictoryAndRemovalRule :: Rule -> Rule
    contradictoryAndRemovalRule (stnums, t) = (stnums, contradictoryAndRemovalTerm t)

    contradictoryAndRemovalTerm :: Term -> Term
    contradictoryAndRemovalTerm (TmIf cnd t1 elseifs t2) = TmIf (contradictoryAndRemovalCond cnd) (contradictoryAndRemovalTerm t1)
        (map (\(c,t) -> (contradictoryAndRemovalCond c, contradictoryAndRemovalTerm t)) elseifs) (contradictoryAndRemovalTerm t2)
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
    cleanAnds ((TmAnd cnds1):cnds2) = cleanAnds $ cnds1 ++ cnds2
    cleanAnds ((TmOr cnds1):cnds2) = cleanAnds cnds2 --TODO think what can we do about ORs
    cleanAnds ((TmEquals v i):cnds) = do
        s <- get
        if contradictsSet s (v,i)
            then return False
            else modify ((v,i):) >> cleanAnds cnds
    cleanAnds [] = return True

    contradictsSet :: [(Variable, Integer)] -> (Variable, Integer) -> Bool
    contradictsSet set (v,i) = case lookup v set of
        Nothing -> False
        Just i2 -> i2 /= i

