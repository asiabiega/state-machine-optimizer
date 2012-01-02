{
{-# OPTIONS_GHC -w #-}
module Lexer where
import Prelude hiding (lex)
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$quote = "

tokens :-
    	$digit+						{ \p s -> tokenWithPos p (TkInt (read s)) }
	IF						{ \p s -> tokenWithPos p TkIf }
	DECISION					{ \p s -> tokenWithPos p TkDecision }
	CASE						{ \p s -> tokenWithPos p TkCase }
	ELSEIF						{ \p s -> tokenWithPos p TkElseIf }
	ARM						{ \p s -> tokenWithPos p TkArm }
	EQUALS						{ \p s -> tokenWithPos p TkEquals }
	AND						{ \p s -> tokenWithPos p TkAnd }
	OR						{ \p s -> tokenWithPos p TkOr }
	VAR						{ \p s -> tokenWithPos p TkVar }
	"_"						{ \p s -> tokenWithPos p TkWildcard }
	"("						{ \p s -> tokenWithPos p TkLParen }
	")"						{ \p s -> tokenWithPos p TkRParen }
	$quote [^$quote]* $quote 			{ \p s -> tokenWithPos p (TkString (read s)) }
	($white)+      		     			;
{
data BaseToken = TkIf
	| TkDecision
	| TkCase
	| TkElseIf
	| TkArm
	| TkEquals
	| TkAnd
	| TkOr
	| TkVar
	| TkWildcard
	| TkLParen
	| TkRParen
	| TkInt Integer
	| TkString String
	deriving (Show, Eq)

type Token = ((Int,Int), BaseToken)

tokenWithPos :: AlexPosn -> BaseToken -> Token
tokenWithPos (AlexPn _ line col) t  = ((line,col),t)

lex = alexScanTokens
}
