module Evaluator where
import Prelude
import Data.List(elem)
import AST

-------------------------------------------------------------------------------------------------------------

getStateTerm :: StateNumber -> Character -> Term
getStateTerm state_num [] = TmDecision TmCurrent "No such state number"
getStateTerm state_num (x:xs) = if elem state_num (fst x) then snd x else getStateTerm state_num xs

runMachine :: StateNumber -> Character -> Env-> (Term, Cost)
runMachine state_num character env = let t = getStateTerm state_num character in eval t env 0

-------------------------------------------------------------------------------------------------------------

eval :: Term -> Env -> Cost -> (Term, Cost)

eval (TmIf cond tt elifs tf) env cost = case evalCondition cond env cost of 
    (TmTrue, c) -> eval tt env (cost+c)
    (TmFalse, c) -> case elifs of 
        [] -> eval tf env (cost+c)
        (cnd, term):xs -> eval (TmIf cnd term xs tf) env (cost+c)

eval (TmCase (TmVar x) arms def) env cost = case lookup x env of
    Just val -> let t = chooseCase val arms def in eval t env (cost+11.5)
    Nothing -> eval def env (cost+11.5)

--TODO: wildcard + jawnie decision
eval t env cost = (t, cost + 4)

-------------------------------------------------------------------------------------------------------------

chooseCase val [] def = def
chooseCase val ((vs, t):xs) def = if elem val vs then t
                                else chooseCase val xs def

-------------------------------------------------------------------------------------------------------------
evalCondition :: Condition -> Env -> Cost -> (Condition, Cost)

evalCondition (TmEquals (TmVar x) val) env cost = case lookup x env of
    Just v -> if v == val then (TmTrue, cost+6.5) else (TmFalse, cost+6.5)
    Nothing -> (TmFalse, cost+6.5)

evalCondition (TmAnd []) env cost = (TmTrue, cost)
evalCondition (TmAnd (x:xs)) env cost = let (cond, c) = evalCondition x env cost in if cond == TmTrue 
                                then evalCondition (TmAnd xs) env (cost+c)
                                else (TmFalse, cost+c)

evalCondition (TmOr []) env cost = (TmFalse, cost)
evalCondition (TmOr (x:xs)) env cost = let (cond, c) = evalCondition x env cost in if cond == TmTrue 
                                then (TmTrue, cost+c)
                                else evalCondition (TmOr xs) env (cost+c)



