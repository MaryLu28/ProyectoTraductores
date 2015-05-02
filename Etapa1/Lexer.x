{------------------------------------------------------------------------------
    Traductores e Interpretadores
    Abril - Junio 2015
    Lanscii Etapa 1 Lexer
    Integrantes: 
    Maria Lourdes Garcia Florez 10-10264
    Sahid Reyes 10-10603
------------------------------------------------------------------------------}

{
	module Lexer
	( 
		lexer, alexScanTokens
	) 
	where
	import Tokens (Pos(..), Token(..), isTokenError, printError)
}

%wrapper "posn"

$digit	= [0-9]---------------> Difitos
$alpha	= [a-zA-Z]------------> Caracteres Alfabeticos
$ascii	= [\x00-\xff]---------> Todos los caracteres de ascii

tokens :-

	----- Espacios y Comentarios -----
	$white+           		;
	"--".*            		;
	"{-"$ascii*"-}"  		;

	----- Braquets -----
	"{"							{ TokenLcurly}
	"}"							{ TokenRcurly}
	"("							{ TokenLparenthesis}
	")"							{ TokenRparenthesis}
	"["							{ TokenLclasp}
	"]"							{ TokenRclasp}

	----- Lienzos -----
	"<"[\/\\\|\-\_\ ]">"		{ TokenCanvas} -----> falta el lienzo vacio

	----- Constantes -----
	true						{ TokenTrue}
	false						{ TokenFalse}
	$digit+						{} ----> no se como poner el token

	----- Variables ------
	$alpha[$alpha $digit\_]* 	{ TokenIdentifier}

	----- Tipos -----
	"%"							{ TokenPercent}
	"@"							{ TokenAt}
	"!"							{ TokenExclamation}

	----- Operadores -----
	"+"							{ TokenPlus}
	"-"							{ TokenMinus}
	"*"							{ TokenMult}
	"/"							{ TokenDiv}
	"/\"						{ TokenAnd}
	"\/"						{ TokenOr}
	"^"							{ TokenNot}
	":"							{ TokenColon}
	","							{ TokenComma}
	"$"							{ TokenRotation}
	"'"							{ TokenTransposition}

	----- Relacionales -----
	"<"							{ TokenLessThan}
	"<="						{ TokenLessEqual}
	">"							{ TokenGreaterThan}
	">="						{ TokenGreaterEqual}
	"="							{ TokenEqual}
	"/="						{ TokenNotEqual}

	----- Separadores -----
	";"							{ TokenSemicolon}
	"|"							{ TokenPipe}
	"?"							{ TokenQuestion}

	----- Entrada y Salida -----
	read						{ TokenRead}
	write						{ TokenWrite}
