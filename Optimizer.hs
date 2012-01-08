module Optimizer where
import Control.Monad.State
import Control.Concurrent.MVar
import Data.List
import System.Random

import Optimizer2
import MachineSize
import AST


optimize :: Character -> Tagger Character
optimize = optimize' (map fst optimizations)

optimize' :: [Character -> Tagger Character] -> Character -> Tagger Character
optimize' (op:ops) char = do
    ochar <- op char
    optimize' ops ochar
optimize' [] char = return char

charToConditionList :: Character -> [(StateNumber, [([(Bool, Condition)], Term)])] --no AND nor OR conditions, terms - only decisions
charToConditionList = undefined

changeOrder :: Character -> TaggerState -> MVar (Integer, Character) -> IO ()
changeOrder char oldState mvar = do
        let clist = charToConditionList char
        randomAst <- randomOrderAst (varsVals char) clist
        let (newAst, newState) = runState (optimize randomAst) oldState
        let newSize = msize newAst
        modifyMVar_ mvar $ \(oldSize, oldAst) -> if oldSize > newSize
            then return (newSize, newAst)
            else return (oldSize, oldAst)
        changeOrder char newState mvar

randomOrderAst :: [VarUniverse] -> [(StateNumber, [([(Bool, Condition)], Term)])] -> IO Character
randomOrderAst vars clists = mapM (randomOrderRule vars) clists where --clist to this rule's list
    randomOrderRule :: [VarUniverse] -> (StateNumber, [([(Bool, Condition)], Term)]) -> IO Rule
    randomOrderRule vars (stateNum, clist) = do
        rterm <- randomOrderTerm vars clist
        return ([stateNum], rterm)

    randomOrderTerm :: [VarUniverse] -> [([(Bool, Condition)], Term)] -> IO Term
    randomOrderTerm [] [([], term)] = return term
    randomOrderTerm (v:vars) clist = do
        ((rvar,rvals), rest) <- randomElemWithRest (v:vars)
        (arms, def) <- armsDefFromClist rvar rvals rest clist
        return $ TmCase (TmVar rvar) arms def
    randomOrderTerm a b = error $ "randomOrderTerm " ++ show a ++ " " ++ show b

    armsDefFromClist var vals rest clist = do
       (arms, clistRest) <- armsFromClist var vals rest clist
       def <- randomOrderTerm rest clistRest
       return (arms, def)

    armsFromClist var vals rest clist = undefined --mapM (\val -> armFromClist var val rest clist) vals

    armFromClist var val rest clist = undefined

optimizations :: [(Character -> Tagger Character, String)]
optimizations = optimizations1 ++ optimizations2

optimizations1 :: [(Character -> Tagger Character, String)]
optimizations1 = [(contradictoryAndRemoval, "contradictory-and-removal")
                 ,(return . stateNumberWildcarder, "state-number-wildcarder")
                 ,(ifCaseInterchange, "if-case-interchange")]

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


ifCaseInterchange :: Character -> Tagger Character
ifCaseInterchange = mapM ifCaseInterchangeRule where
    ifCaseInterchangeRule :: Rule -> Tagger Rule
    ifCaseInterchangeRule (stateNums, term) = do { mt <- ifCaseInterchangeTerm term; return (stateNums, mt)}

    ifCaseInterchangeTerm :: Term -> Tagger Term
    ifCaseInterchangeTerm tif@(TmIf cnd _ elseifs _) = do
        mtcase <- caseFromIf tif
        return $ case mtcase of
            tcase@(TmCase _ arms _) -> if msizeCondition cnd + sum (map (msizeCondition . fst) elseifs) > 10 + mspan arms
                then tcase
                else tif
            _ -> tif

    ifCaseInterchangeTerm tcase@(TmCase _ arms _) = do
        mtif <- ifFromCase tcase
        return $ case mtif of
            tif@(TmIf cnd _ elseifs _) -> if msizeCondition cnd + sum (map (msizeCondition . fst) elseifs) > 10 + mspan arms
                then tcase
                else tif
            _ -> tcase

    ifCaseInterchangeTerm a = return a

    caseFromIf :: Term -> Tagger Term
    caseFromIf tif@(TmIf cnd t1 elseifs t2) = case cleanIf tif of
        Nothing -> return tif
        Just v -> let marms = armsFromElseifs ((cnd,t1):elseifs) in return $ TmCase (TmVar v) marms t2
    caseFromIf a = error $ "caseFromIf " ++ show a

    armsFromElseifs :: [(Condition, Term)] -> [(ValueSet, Term)]
    armsFromElseifs ((TmEquals _ val _, term):elseifs) = ([val], term) : armsFromElseifs elseifs
    armsFromElseifs ((TmAnd cnds _, term):elseifs) = (valueSetFromCnds cnds, term) : armsFromElseifs elseifs
    armsFromElseifs ((TmOr cnds _, term):elseifs) = (valueSetFromCnds cnds, term) : armsFromElseifs elseifs
    armsFromElseifs [] = []
    armsFromElseifs a = error $ "armsFromElseIfs " ++ show a
    --TODO it has to have false ands removal immediately before
    --TODO maybe split ands to those, who can be transformed

    valueSetFromCnds :: [Condition] -> [Integer]
    valueSetFromCnds (TmEquals _ v _:cs) = v : valueSetFromCnds cs
    valueSetFromCnds (TmFalse:cs) = valueSetFromCnds cs
    valueSetFromCnds (TmTrue:cs) = valueSetFromCnds cs
    valueSetFromCnds (TmOr cnds _:cs) = valueSetFromCnds (cnds ++ cs)
    valueSetFromCnds (TmAnd cnds _:cs) = valueSetFromCnds (cnds ++ cs)
    valueSetFromCnds [] = []

    -- | has only one variable in conditions
    cleanIf :: Term -> Maybe String
    cleanIf (TmIf cnd _ elseifs _) = case vunion $ varsCnd cnd : map (varsCnd . fst) elseifs of
        [v] -> Just v
        _ -> Nothing
    cleanIf a = error $ "cleanIf " ++ show a

    ifFromCase :: Term -> Tagger Term
    ifFromCase (TmCase var arms def) = case simplifyArms arms of
        [] -> return def
        (vals, term):as -> do
            mor <- orFromVals var vals
            mei <- elseifsFromArms var as
            return $ TmIf mor term mei def
    ifFromCase a = error $ "ifFromCase " ++ show a

    orFromVals :: Variable -> [Integer] -> Tagger Condition
    orFromVals var vs = do
        mcnds <- mapM (\v -> cachedCondition $ TmEquals var v 0) vs
        cachedCondition $ TmOr mcnds 0

    elseifsFromArms :: Variable -> [(ValueSet, Term)] -> Tagger [(Condition, Term)]
    elseifsFromArms var ((vals, term):as) = do
        mh <- orFromVals var vals
        mt <- elseifsFromArms var as
        return $ (mh, term) : mt
    elseifsFromArms _ [] = return []

    simplifyArms ((ns, t):arms) = map (\i -> ([i], t)) ns ++ simplifyArms arms
    simplifyArms [] = []


randomIndex :: [a] -> IO Int
randomIndex l = randomRIO (0, length l - 1)

randomElem :: [a] -> IO a
randomElem l = do
    ridx <- randomIndex l
    return $ l !! ridx

randomElemWithRest :: Eq a => [a] -> IO (a, [a])
randomElemWithRest els = do
    rel <- randomElem els
    return (rel, delete rel els)
