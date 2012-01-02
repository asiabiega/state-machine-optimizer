module AST where

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
