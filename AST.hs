module AST where

import Data.List
import Data.Function

data Term = TmIf Condition Term [(Condition, Term)] Term
            | TmDecision NewState Utterance
            | TmCase Variable [(ValueSet, Term)] Term
            deriving(Eq, Show)

data Condition = TmEquals Variable Integer
            | TmAnd [Condition]
            | TmOr [Condition]
            | TmTrue
            | TmFalse
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
vars char = map fst (varsVals char)

varsVals :: Character -> [(String, [Integer])]
varsVals ch = map (foldl (\(_,ts) (b, t) -> (b,t:ts)) (error "empty group", [])) $ groupBy (on (==) fst) $ sort $ varsVals' ch where
    varsVals' :: Character -> [(String, Integer)]
    varsVals' rules = vunion $ map varsRule rules

    varsRule (_, term) = varsTerm term

    varsTerm (TmIf cnd t1 elseifs t2) = vunion $ map varsTerm ([t1, t2] ++ map snd elseifs) ++ map varsCnd (cnd : map fst elseifs)
    varsTerm (TmDecision _ _) = []
    varsTerm (TmCase (TmVar v) arms t) = map (\t -> (v,t)) (concatMap fst arms)  ++  vunion (map varsTerm $ t : map snd arms)

    varsCnd (TmEquals (TmVar v) i) = [(v, i)]
    varsCnd (TmAnd cnds) = vunion $ map varsCnd cnds
    varsCnd (TmOr cnds) = vunion $ map varsCnd cnds
    varsCnd TmTrue = []
    varsCnd TmFalse = []

vunion :: Eq a => [[a]] -> [a]
vunion = foldl' union []
