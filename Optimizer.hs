module Optimizer where
import Control.Arrow
import Control.Monad.State
import Control.Concurrent.MVar
import Data.List

import Optimizer2
import MachineSize
import AST

changeOrder :: Character -> MVar (Integer, Character) -> IO ()
changeOrder char mvar = forever $ do
        let clist = charToConditionList char
        newAst <- fmap fastOptimizations (randomOrderAst clist)
        let newSize = msize newAst
        modifyMVar_ mvar $ \(oldSize, oldAst) -> if oldSize > newSize then return (newSize, newAst) else return (oldSize, oldAst)

charToConditionList :: Character -> [([Condition], Term)] --no AND nor OR conditions, terms - only decisions
charToConditionList = undefined

randomOrderAst :: [([Condition], Term)] -> IO Character
randomOrderAst = undefined

fastOptimizations :: Character -> Character
fastOptimizations = fixOptimizations $ foldl' (.) id (map fst $ optimizations ++ optimizations2)

optimizations :: [(Character -> Character, String)]
optimizations = [(contradictoryAndRemoval, "contradictory-and-removal")
                ,(stateNumberWildcarder, "state-number-wildcarder")
                ,(ifCaseInterchange, "if-case-interchange")]


-- | Applies given optimization, until a fixpoint is reached
fixOptimizations :: (Character -> Character) -> Character -> Character
fixOptimizations opt char = let ochar = opt char in
    if ochar == char
        then ochar
        else fixOptimizations opt ochar

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

ifCaseInterchange :: Character -> Character
ifCaseInterchange = map ifCaseInterchangeRule where
    ifCaseInterchangeRule (stateNums, term) = (stateNums, ifCaseInterchangeTerm term)

    ifCaseInterchangeTerm tif@(TmIf cnd _ elseifs _) = case caseFromIf tif of
        tcase@(TmCase _ arms _) -> if msizeCondition cnd + sum (map (msizeCondition . fst) elseifs) > 10 + mspan arms
            then tcase
            else tif
        _ -> tif

    ifCaseInterchangeTerm tcase@(TmCase _ arms _) = case ifFromCase tcase of
        tif@(TmIf cnd _ elseifs _) -> if msizeCondition cnd + sum (map (msizeCondition . fst) elseifs) > 10 + mspan arms
            then tcase
            else tif
        _ -> tcase

    ifCaseInterchangeTerm a = a

    caseFromIf tif@(TmIf cnd t1 elseifs t2) = maybe tif
        (\v -> TmCase (TmVar v) (armsFromElseifs ((cnd,t1):elseifs)) t2) (cleanIf tif)

    armsFromElseifs ((TmEquals _ val, term):elseifs) = ([val], term) : armsFromElseifs elseifs
    armsFromElseifs ((TmAnd cnds, term):elseifs) = (valueSetFromCnds cnds, term) : armsFromElseifs elseifs
    armsFromElseifs ((TmOr cnds, term):elseifs) = (valueSetFromCnds cnds, term) : armsFromElseifs elseifs
    armsFromElseifs [] = []
    --TODO it has to have false ands removal immediately before
    --TODO maybe split ands to those, who can be transformed

    valueSetFromCnds :: [Condition] -> [Integer]
    valueSetFromCnds (TmEquals _ v:cs) = v : valueSetFromCnds cs
    valueSetFromCnds (TmFalse:cs) = valueSetFromCnds cs
    valueSetFromCnds (TmTrue:cs) = valueSetFromCnds cs
    valueSetFromCnds (TmOr cnds:cs) = valueSetFromCnds (cnds ++ cs)
    valueSetFromCnds (TmAnd cnds:cs) = valueSetFromCnds (cnds ++ cs)
    valueSetFromCnds [] = []

    -- | has only one variable in conditions
    cleanIf :: Term -> Maybe String
    cleanIf (TmIf cnd _ elseifs t2) = case vunion $ varsCnd cnd : map (varsCnd . fst) elseifs of
        [v] -> Just v
        _ -> Nothing

    ifFromCase (TmCase var arms def) = case simplifyArms arms of
        [] -> def
        (vals, term):as -> TmIf (orFromVals var vals) term (elseifsFromArms var as) def

    orFromVals :: Variable -> [Integer] -> Condition
    orFromVals var vs = TmOr (map (\v -> TmEquals var v) vs)

    elseifsFromArms var ((vals, term):as) = (orFromVals var vals, term) : elseifsFromArms var as
    elseifsFromArms _ [] = []

    simplifyArms (([n], t):arms) = ([n], t) : simplifyArms arms
    simplifyArms ((n:ns, t):arms) = map (\i -> ([i], t)) (n:ns) ++ simplifyArms arms
    simplifyArms [] = []
