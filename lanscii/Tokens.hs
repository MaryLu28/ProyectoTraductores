{------------------------------------------------------------------------------
    Traductores e Interpretadores
    Abril - Junio 2015
    Lanscii Etapa 1 Tokens
    Integrantes: 
    Maria Lourdes Garcia Florez 10-10264
    Sahid Reyes 10-10603
------------------------------------------------------------------------------}
{
module Token (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}
tok f p s = f p s
data Token = 
		----- Brackets ----
			TokenLcurly	
			| TokenRcurly 
			| TokenLparenthesis 
			| TokenRparenthesis 
			| TokenLclasp 
			| TokenRclasp
			
		----- Lienzos -----
			| TokenCanvas

		----- Constantes ----
			| TokenTrue
			| TokenFalse

		----- Operadores ---			 
			| TokenPlus
			| TokenMinus
			| TokenMult
			| TokenDiv
			| TokenAnd
			| TokenOr
			| TokenNot
			| TokenColon
			| TokenComma
			| TokenRotation
			| TokenTransposition

		----- Relacionales -----
			| TokenLessThan
			| TokenLessEqual
			| TokenGreaterThan
			| TokenGreaterEqual
			| TokenEqual
			| TokenNotEqual

		----- Separadores -----
			| TokenSemicolon
			| TokenPipe
			| TokenQuestion

		----- Entrada y Salida -----
			| TokenRead
			| TokenWrite




			|Sym AlexPosn Char 
			|Var AlexPosn String 
			|Int AlexPosn Int
			deriving (Eq,Show)

token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p
}
