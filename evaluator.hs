import Prelude
import Data.List(elem)

-------------------------------------------------------------------------------------------------------------

data Term = TmIf Condition Term [(Condition, Term)] Term
            | TmDecision NewState Utterance
            | TmCase Variable [(ValueSet, Term)] Term
            deriving(Eq, Show)

data Condition = TmEquals Variable Int | TmAnd [Condition] | TmOr [Condition] | TmTrue | TmFalse
            deriving(Eq, Show)

data NewState = TmCurrent | TmState Int
            deriving(Eq, Show)

data Variable = TmVar String
            deriving(Eq, Show)

type StateNumber = Int
type ValueSet = [Int]
type Utterance = String

type Rule = ([StateNumber], Term)
type Character = [Rule]
type Env = [(String, Int)]

-------------------------------------------------------------------------------------------------------------

getStateTerm :: StateNumber -> Character -> Term
getStateTerm state_num [] = TmDecision TmCurrent "No such state number"
getStateTerm state_num (x:xs) = if elem state_num (fst x) then snd x else getStateTerm state_num xs

runMachine :: StateNumber -> Character -> Env-> Term
runMachine state_num character env = let t = getStateTerm state_num character in eval t env

-------------------------------------------------------------------------------------------------------------

eval :: Term -> Env-> Term

eval (TmIf cond tt elifs tf) env = case evalCondition cond env of 
    TmTrue -> eval tt env
    TmFalse -> case elifs of 
        [] -> eval tf env
        (cnd, term):xs -> eval (TmIf cnd term xs tf) env

eval (TmCase (TmVar x) arms def) env = case lookup x env of
    Just val -> let t = chooseCase val arms def in eval t env
    Nothing -> eval def env

eval t env = t

-------------------------------------------------------------------------------------------------------------

chooseCase val [] def = def
chooseCase val ((vs, t):xs) def = if elem val vs then t
                                else chooseCase val xs def

-------------------------------------------------------------------------------------------------------------
evalCondition :: Condition -> Env -> Condition

evalCondition (TmEquals (TmVar x) val) env = case lookup x env of
    Just v -> if v == val then TmTrue else TmFalse
    Nothing -> TmFalse

evalCondition (TmAnd []) env = TmTrue
evalCondition (TmAnd (x:xs)) env = if evalCondition x env == TmTrue then evalCondition (TmAnd xs) env
                                else TmFalse

evalCondition (TmOr []) env = TmFalse
evalCondition (TmOr (x:xs)) env = if evalCondition x env == TmTrue then TmTrue
                                else evalCondition (TmOr xs) env
