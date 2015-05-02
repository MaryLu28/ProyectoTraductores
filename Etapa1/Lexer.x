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

	$white+           		;----------> Espacios
	"--".*            		;----------> Comentarios de Linea
	"{-"$ascii*"-}"  		;----------> Comentarios de Bloque