module AST where

import Data.List
import Data.Function
import Text.PrettyPrint

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

pp :: Character -> String
pp = render . ppChar

ppChar :: Character -> Doc
ppChar rules = parens . nest 1 . vcat . map ppRule $ rules

ppRule :: Rule -> Doc
ppRule (statenums, term) = parens $ ppStateNums statenums <+> ppTerm term

ppStateNums :: [StateNumber] -> Doc
ppStateNums = hsep . map integer

ppTerm :: Term -> Doc
ppTerm (TmIf cond term elseifs term2) = parens $ text "IF" <+> ppCond cond $$
    nest 1 (ppTerm term
    $$ parens (vcat $ map ppElseif elseifs) $$ ppTerm term2)
ppTerm (TmDecision (TmState newstate) utterance) = parens $ text "DECISION" <+> integer newstate <+> doubleQuotes (text utterance)
ppTerm (TmDecision TmCurrent utterance)          = parens $ text "DECISION" <+> text "_"         <+> doubleQuotes (text utterance)
ppTerm (TmCase var arms term) = parens $ text "CASE" <+> ppVar var <+> parens (vcat $ map ppArm arms) <+> nest 1 (ppTerm term)

ppVar :: Variable -> Doc
ppVar (TmVar v) = parens $ text "VAR" <+> doubleQuotes (text v)

ppCond :: Condition -> Doc
ppCond (TmEquals var i) = parens $ text "EQUALS" <+> ppVar var <+> integer i
ppCond (TmAnd conds) = parens $ text "AND" <+> vcat (map ppCond conds)
ppCond (TmOr conds)  = parens $ text "OR"  <+> vcat (map ppCond conds)

ppArm :: (ValueSet, Term) -> Doc
ppArm (vs, t) = parens $ text "ARM" <+> parens (vcat $ map integer vs) $$ ppTerm t

ppElseif :: (Condition, Term) -> Doc
ppElseif (c, t) = parens $ text "ELSEIF" <+> ppCond c $$ nest 1 (ppTerm t)

startingStates :: Character -> [StateNumber]
startingStates = concatMap fst

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
