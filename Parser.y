{
{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import AST
}

%name parse character

%tokentype { Token }
%error     { parseError }

%token
	And             { (_,TkAnd)	}
	LParen		{ (_,TkLParen)	}
	RParen		{ (_,TkRParen)  }
	If		{ (_,TkIf)	}
	Decision	{ (_,TkDecision)}
	Case		{ (_,TkCase)	}
	ElseIf		{ (_,TkElseIf)	}
	Int		{ (_,TkInt $$)	}
	String		{ (_,TkString $$)}
	Equals		{ (_,TkEquals)  }
	Wildcard	{ (_,TkWildcard)}
	Or		{ (_,TkOr)	}
	Var		{ (_,TkVar)	}
	Arm		{ (_,TkArm)	}
%%

character :: { Character }
character: 	LParen rules RParen			 { $2 }

rules :: { [Rule] }
rules:		rule rules				 { $1 : $2 }
     		| 					 { [] }

rule :: { Rule }
rule:		LParen statenumbers statement RParen 	 { ($2, $3) }

statement :: { Term }
statement:	LParen If condition statement LParen
	 		elseifs RParen statement RParen  { TmIf $3 $4 $6 $8 }
	 	| LParen Decision newstate utterance
			RParen 				 { TmDecision $3 $4 }
		| LParen Case variable LParen arms
			RParen statement RParen		 { TmCase $3 $5 $7 }

elseifs :: { [(Condition, Term)] }
elseifs:	elseif elseifs				 { $1 : $2 }
       		| 					 { [] }

elseif :: { (Condition, Term) }
elseif:		LParen ElseIf condition statement RParen { ($3, $4) }

arms :: { [(ValueSet, Term)] }
arms:		arm arms				 { $1 : $2 }
    		|					 { [] }

arm :: { (ValueSet, Term) }
arm:		LParen Arm valueset statement RParen	 { ($3, $4) }

condition :: { Condition }
condition:	LParen Equals variable Int RParen	 { TmEquals $3 $4 }
	 	| LParen And conditions RParen		 { TmAnd $3 }
		| LParen Or conditions RParen		 { TmOr $3 }

conditions ::{ [Condition] }
conditions:	condition conditions			 { $1 : $2 }
	  	|					 { [] }

variable :: { Variable }
variable:	LParen Var String RParen		 { TmVar $3 }

statenumbers :: { [StateNumber] }
statenumbers:	statenumber statenumbers		 { $1 : $2 }
	    	| 					 { [] }

statenumber :: { StateNumber }
statenumber:	Int					 { $1 }

newstate :: { NewState }
newstate:	Int					 { TmState $1 }
		| Wildcard				 { TmCurrent }

utterance :: { Utterance }
utterance:	String					 { $1 }

values ::	{ [Integer] }
values:		Int values				 { $1 : $2 }
      		|					 { [] }

valueset :: { ValueSet }
valueset:	LParen values RParen	 		 { $2 }

{
parseError :: [Token] -> a
parseError (((line,col),t):xs) = error $ "Parse error at token " ++ show t ++", line " ++ (show line) ++ ", column " ++ (show col)
parseError [] = error "Parse error at the end"
}
