module AST where

data Term = TmIf Condition Term [(Condition, Term)] Term
            | TmDecision NewState Utterance
            | TmCase Variable [(ValueSet, Term)] Term
            deriving(Eq, Show)

data Condition = TmEquals Variable Integer | TmAnd [Condition] | TmOr [Condition] | TmTrue | TmFalse
            deriving(Eq, Show)

data NewState = TmCurrent | TmState Integer
            deriving(Eq, Show)

data Variable = TmVar String
            deriving(Eq, Show)

type StateNumber = Integer
type ValueSet = [Integer]
type Utterance = String
type Cost = Float

type Rule = ([StateNumber], Term)
type Character = [Rule]
type Env = [(String, Int)]
