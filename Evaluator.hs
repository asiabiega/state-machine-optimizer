-- @author: Joanna Biega
-- machine evaluation module

module Evaluator where
import Prelude hiding (lex)
import System.Random

import AST

-------------------------------------------------------------------------------------------------------------

getStateTerm :: StateNumber -> Character -> Term
getStateTerm state_num [] = TmDecision TmCurrent "No such state number"
getStateTerm state_num (x:xs) = if elem state_num (fst x) then snd x else getStateTerm state_num xs

runMachine :: StateNumber -> Character -> Env -> (Term, Cost)
runMachine state_num character env = let t = getStateTerm state_num character 
        in let (term, cost) = eval t env 0 in 
        case term of
            TmDecision TmCurrent s -> (TmDecision (TmState state_num) s, cost)
            _ -> (term, cost)

-------------------------------------------------------------------------------------------------------------

eval :: Term -> Env -> Cost -> (Term, Cost)

eval (TmIf cond tt elifs tf) env cost = case evalCondition cond env cost of 
    (TmTrue, c) -> eval tt env c
    (TmFalse, c) -> case elifs of 
        [] -> eval tf env c
        (cnd, term):xs -> eval (TmIf cnd term xs tf) env c

eval (TmCase (TmVar x) arms def) env cost = case lookup x env of
    Just val -> let t = chooseCase val arms def in eval t env (cost+11.5)
    Nothing -> eval def env (cost+11.5)

eval (TmDecision TmCurrent s) env cost = (TmDecision TmCurrent s, cost + 3)
eval (TmDecision t s) env cost = (TmDecision t s, cost + 4)

-------------------------------------------------------------------------------------------------------------

chooseCase val [] def = def
chooseCase val ((vs, t):xs) def = if elem val vs then t
                                else chooseCase val xs def

-------------------------------------------------------------------------------------------------------------
evalCondition :: Condition -> Env -> Cost -> (Condition, Cost)

evalCondition (TmEquals (TmVar x) val _) env cost = case lookup x env of
    Just v -> if v == val then (TmTrue, cost+6.5) else (TmFalse, cost+6.5)
    Nothing -> (TmFalse, cost+6.5)

evalCondition (TmAnd [] _) env cost = (TmTrue, cost)
evalCondition (TmAnd (x:xs) _) env cost = let (cond, c) = evalCondition x env cost in if cond == TmTrue 
                                then evalCondition (TmAnd xs 0) env c
                                else (TmFalse, c)

evalCondition (TmOr [] _) env cost = (TmFalse, cost)
evalCondition (TmOr (x:xs) _) env cost = let (cond, c) = evalCondition x env cost in if cond == TmTrue 
                                then (TmTrue, c)
                                else evalCondition (TmOr xs 0) env c

evalCondition TmTrue _ cost = (TmTrue, cost)
evalCondition TmFalse _ cost = (TmFalse, cost)
--------------------------------------------------------------------------------------------------------------
randomEnv :: Character -> IO Env
randomEnv char = do
    let vv = varsVals char
    mapM (\(vr, vls) -> randomRIO (0, length vls - 1) >>= \ridx -> return (vr, vls !! ridx)) vv
