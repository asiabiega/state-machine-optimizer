import Prelude
import Data.List(elem)

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

get_state_term :: StateNumber -> Character -> Term
get_state_term state_num [] = TmDecision TmCurrent "No such state number"
get_state_term state_num (x:xs) = if elem state_num (fst x) then snd x else get_state_term state_num xs

run_machine :: StateNumber -> Character -> Env-> Term
run_machine state_num character env = let t = get_state_term state_num character in eval t env

--get_env_val

eval :: Term -> Env-> Term

eval (TmIf cond tt elifs tf) env = case eval_condition cond env of 
    TmTrue -> eval tt env
    TmFalse -> case elifs of 
        [] -> eval tf env
        (cnd, term):xs -> eval (TmIf cnd term xs tf) env

eval (TmCase x [] def) env = eval def env
eval (TmCase (TmVar x) ((vs, t):xs) def) env = eval def env -- if elem (env x) vs then eval t else eval rec.

eval t env = t

eval_condition :: Condition -> Env -> Condition

--tutaj booleans
eval_condition (TmEquals (TmVar x) val) env = TmTrue -- if elem_val (env x) == k then True else False

eval_condition (TmAnd []) env = TmTrue
eval_condition (TmAnd (x:xs)) env = if eval_condition x env == TmTrue then eval_condition (TmAnd xs) env
                                else TmFalse

eval_condition (TmOr []) env = TmFalse
eval_condition (TmOr (x:xs)) env = if eval_condition x env == TmTrue then TmTrue
                                else eval_condition (TmOr xs) env
