module Optimizer where
import Control.Monad.State
import Control.Concurrent.MVar
import Data.List

import Optimizer2
import MachineSize
import AST


changeOrder :: Character -> TaggerState -> MVar (Integer, Character) -> IO ()
changeOrder char state mvar = do
        let clist = charToConditionList char
        randomAst <- randomOrderAst clist
        let (newAst, newState) = runState (optimize randomAst) state
        let newSize = msize newAst
        modifyMVar_ mvar $ \(oldSize, oldAst) -> if oldSize > newSize
            then return (newSize, newAst)
            else return (oldSize, oldAst)
        changeOrder char newState mvar

optimize :: Character -> Tagger Character
optimize = optimize' (map fst optimizations)

optimize' :: [Character -> Tagger Character] -> Character -> Tagger Character
optimize' (op:ops) char = do
    ochar <- op char
    optimize' ops ochar
optimize' [] char = return char

charToConditionList :: Character -> [([Condition], Term)] --no AND nor OR conditions, terms - only decisions
charToConditionList = undefined

randomOrderAst :: [([Condition], Term)] -> IO Character
randomOrderAst = undefined

optimizations :: [(Character -> Tagger Character, String)]
optimizations = optimizations1 ++ optimizations2

optimizations1 :: [(Character -> Tagger Character, String)]
optimizations1 = [(contradictoryAndRemoval, "contradictory-and-removal")]
--                ,(stateNumberWildcarder, "state-number-wildcarder")
--                ,(ifCaseInterchange, "if-case-interchange")]

-- | Applies given optimization, until a fixpoint is reached
fixOptimizations :: (Character -> Character) -> Character -> Character
fixOptimizations opt char = let ochar = opt char in
    if ochar == char
        then ochar
        else fixOptimizations opt ochar


-- | Contradictory and condition removal, it changes the whole condition to TmFalse, when TmAnd conditions are contradictory
contradictoryAndRemoval :: Character -> Tagger Character
contradictoryAndRemoval = mapM contradictoryAndRemovalRule where
    contradictoryAndRemovalRule :: Rule -> Tagger Rule
    contradictoryAndRemovalRule (stnums, t) = do { mt <- contradictoryAndRemovalTerm t; return (stnums, mt)}

    contradictoryAndRemovalTerm :: Term -> Tagger Term
    contradictoryAndRemovalTerm (TmIf cnd t1 elseifs t2) = do
        mcnd <- contradictoryAndRemovalCond cnd
        mt1 <- contradictoryAndRemovalTerm t1
        mtei <- mapM (\(eic, eit) -> do { meic <- contradictoryAndRemovalCond eic;
                                          meit <- contradictoryAndRemovalTerm eit;
                                          return (meic, meit)}) elseifs
        mt2 <- contradictoryAndRemovalTerm t2
        return $ TmIf mcnd mt1 mtei mt2

    contradictoryAndRemovalTerm (TmCase var arms t) = do
        marms <- mapM (\(vs, at) -> do { mat <- contradictoryAndRemovalTerm at; return (vs, mat)}) arms
        mt <- contradictoryAndRemovalTerm t
        return $ TmCase var marms mt
    contradictoryAndRemovalTerm a@(TmDecision _ _) = return a

    contradictoryAndRemovalCond :: Condition -> Tagger Condition
    contradictoryAndRemovalCond (TmAnd cnds _) = do
        newCnds <- mapM contradictoryAndRemovalCond cnds
        if evalState (cleanAnds newCnds) []
            then cachedCondition $ TmAnd newCnds 0
            else return TmFalse
    contradictoryAndRemovalCond (TmOr cnds _) = do
        newCnds <- mapM contradictoryAndRemovalCond cnds
        cachedCondition $ TmOr newCnds 0
    contradictoryAndRemovalCond a = return a

    cleanAnds :: [Condition] -> State [(Variable, Integer)] Bool
    cleanAnds (TmTrue:cnds) = cleanAnds cnds
    cleanAnds (TmFalse:_) = return False
    cleanAnds (TmAnd cnds1 _:cnds2) = cleanAnds $ cnds1 ++ cnds2
    cleanAnds (TmOr _ _:cnds2) = cleanAnds cnds2 --TODO think what can we do about ORs
    cleanAnds (TmEquals v i _:cnds) = do
        s <- get
        if contradictsSet s (v,i)
            then return False
            else modify ((v,i):) >> cleanAnds cnds
    cleanAnds [] = return True

    contradictsSet :: [(Variable, Integer)] -> (Variable, Integer) -> Bool
    contradictsSet set (v,i) = case lookup v set of
        Nothing -> False
        Just i2 -> i2 /= i

{-
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
-}
