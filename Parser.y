{
{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import AST
}

%name parse funlist

%tokentype { Token }
%error     { parseError }

%token
	And             { (_,TkAnd)	}

%%

character :: Character
character: 	LParen rules RParen			 { $2 }

rules :: [Rule]
rules:		rule rules				 { $1 : $2 }
     		| 					 { [] }

rule :: Rule
rule:		LParen statenumbers statement RParen 	 { ($2, $3) }

statement :: Term
statement:	LParen If condition statement LParen
	 		elseifs RParen statement RParen  { TmIf $3 $4 $6 $8 }
	 	| LParen Decision newstate utterance  	 { TmDecision $3 $4 }
		| LParen Case variable LParen arms
			RParen statemet RParen		 { TmCase $3 $5 $7 }

elseifs :: [(Condition, Term)]
elseifs:	elseif elseifs				 { $1 : $2 }
       		| 					 { [] }

elseif :: (Condition, Term)
elseif:		LParen Elseif condition statement RParen { ($3, $4) }

arms :: [(ValueSet, Term)]
arms:		arm arms				 { $1 : $2 }
    		|					 { [] }

arm :: (ValueSet, Term)
arm:		LParen valueset statement RParen	 { ($2, $3) }

variable :: Variable
variable:	LParen Var String RParen		 { TmVar $3 }

statenumbers :: [StateNumber]
statenumbers:	statenumber statenumbers		 { $1 : $2 }
	    	| 					 { [] }

statenumber :: StateNumber
statenumber	Int					 { $1 }

newstate :: NewState
newstate:	Int					 { TmState $1 }
		| Wildcard				 { TmCurrent }

utterance :: Utterance
utterance:	String					 { $1 }

{
parseError :: [Token] -> a
parseError (((line,col),t):xs) = error $ "Parse error at line " ++ (show line) ++ ", column " ++ (show col)
parseError [] = error "Parse error at the end"
}
