module AST where

import Data.List
import Data.Function
import Text.PrettyPrint
import Control.Monad.State
import qualified Data.Map as Map


type TaggerState = (Int, Map.Map Condition Int)
type Tagger a = State TaggerState a

tagStart :: TaggerState
tagStart = (0, Map.empty)

data Term = TmIf Condition Term [(Condition, Term)] Term
            | TmDecision NewState Utterance
            | TmCase Variable [(ValueSet, Term)] Term
            deriving(Eq, Show)

type EqTag = Int

data Condition = TmEquals Variable Integer EqTag
            | TmAnd [Condition] EqTag
            | TmOr [Condition] EqTag
            | TmTrue
            | TmFalse
            deriving(Show, Ord)

changeEqTag :: Condition -> Int -> Condition
changeEqTag (TmEquals a b _) t = TmEquals a b t
changeEqTag (TmAnd cnds _) t = TmAnd cnds t
changeEqTag (TmOr cnds _) t = TmOr cnds t
changeEqTag a _ = error $ "changeEqTag " ++ show a

cachedCondition :: Condition -> Tagger Condition
cachedCondition cond = do
    (num, m) <- get
    case Map.lookup cond m of
        Just i -> return $ changeEqTag cond i
        Nothing -> do
            put (num+1, Map.insert cond num m)
            return $ changeEqTag cond num

instance Eq Condition where
    TmEquals _ _ t == TmEquals _ _ t1 = t == t1
    TmAnd _ t      == TmAnd _ t1      = t == t1
    TmOr _ t       == TmOr _ t1       = t == t1
    TmTrue         == TmTrue          = True
    TmFalse        == TmFalse         = True
    _              == _               = False

data NewState = TmCurrent | TmState Integer
            deriving(Eq, Show)

data Variable = TmVar String
            deriving(Eq, Show, Ord)

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
ppCond (TmEquals var i _) = parens $ text "EQUALS" <+> ppVar var <+> integer i
ppCond (TmAnd conds _) = parens $ text "AND" <+> vcat (map ppCond conds)
ppCond (TmOr conds _)  = parens $ text "OR"  <+> vcat (map ppCond conds)
ppCond a = error $ "ppCond wrong optimization order, remove TmTrue and TmFalse " ++ show a

ppArm :: (ValueSet, Term) -> Doc
ppArm (vs, t) = parens $ text "ARM" <+> parens (vcat $ map integer vs) $$ ppTerm t

ppElseif :: (Condition, Term) -> Doc
ppElseif (c, t) = parens $ text "ELSEIF" <+> ppCond c $$ nest 1 (ppTerm t)

startingStates :: Character -> [StateNumber]
startingStates = concatMap fst

vars :: Character -> [String]
vars chr = map fst (varsVals chr)

varsVals :: Character -> [(String, [Integer])]
varsVals ch = map (foldl (\(_,ts) (b, t) -> (b,t:ts)) (error "empty group", [])) $ groupBy (on (==) fst) $ sort $ varsVals' ch where
    varsVals' :: Character -> [(String, Integer)]
    varsVals' rules = vunion $ map varsValsRule rules

    varsValsRule (_, term) = varsValsTerm term

    varsValsTerm (TmIf cnd t1 elseifs t2) = vunion $ map varsValsTerm ([t1, t2] ++ map snd elseifs) ++ map varsValsCnd (cnd : map fst elseifs)
    varsValsTerm (TmDecision _ _) = []
    varsValsTerm (TmCase (TmVar v) arms t1) = map (\t -> (v,t)) (concatMap fst arms)  ++  vunion (map varsValsTerm $ t1 : map snd arms)

varsCnd :: Condition -> [String]
varsCnd cnd = map fst (varsValsCnd cnd)

varsValsCnd :: Condition -> [(String, Integer)]
varsValsCnd (TmEquals (TmVar v) i _) = [(v, i)]
varsValsCnd (TmAnd cnds _) = vunion $ map varsValsCnd cnds
varsValsCnd (TmOr cnds _) = vunion $ map varsValsCnd cnds
varsValsCnd TmTrue = []
varsValsCnd TmFalse = []

vunion :: Eq a => [[a]] -> [a]
vunion = foldl' union []
