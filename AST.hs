module AST where

import Data.List

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
type Env = [(String, Integer)]


vars :: Character -> [String]
vars rules = vunion $ map varsRule rules where
    varsRule (_, term) = varsTerm term

    varsTerm (TmIf cnd t1 elseifs t2) = vunion $ map varsTerm ([t1, t2] ++ map snd elseifs) ++ map varsCnd (cnd : map fst elseifs)
    varsTerm (TmDecision _ _) = []
    varsTerm (TmCase (TmVar v) arms t) = v : (vunion $ map varsTerm $ t : map snd arms)

    varsCnd (TmEquals (TmVar v) _) = [v]
    varsCnd (TmAnd cnds) = vunion $ map varsCnd cnds
    varsCnd (TmOr cnds) = vunion $ map varsCnd cnds
    varsCnd TmTrue = []
    varsCnd TmFalse = []

vunion :: Eq a => [[a]] -> [a]
vunion = foldl' union []
