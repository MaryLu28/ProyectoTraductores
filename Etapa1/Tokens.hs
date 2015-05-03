{------------------------------------------------------------------------------
    Traductores e Interpretadores
    Abril - Junio 2015
    Lanscii Etapa 1 Tokens
    Integrantes: 
    Maria Lourdes Garcia Florez 10-10264
    Sahid Reyes 10-10603
------------------------------------------------------------------------------}
module Tokens
( Pos(..)
, Token(..)
, isTokenError
, printError
, tp
) where

data Pos = Pos Int Int deriving Eq
instance Show Pos where
    show (Pos l c) = "(Line " ++ show l ++ ", Col " ++ show c ++ ")"

data Token = 
	
	----- Brackets ----
		  TokenLcurly Pos
		| TokenRcurly Pos
		| TokenLparenthesis Pos 
		| TokenRparenthesis Pos
		| TokenLclasp Pos
		| TokenRclasp Pos
		
	----- Lienzos -----
		| TokenCanvas Pos

	----- Constantes ----
		| TokenTrue Pos
		| TokenFalse Pos

	----- Operadores ---			 
		| TokenPlus Pos
		| TokenMinus Pos
		| TokenMult Pos
		| TokenDiv Pos
		| TokenAnd Pos
		| TokenOr Pos
		| TokenNot Pos
		| TokenColon Pos
		| TokenComma Pos
		| TokenRotation Pos 
		| TokenTransposition Pos

	----- Relacionales -----
		| TokenLessThan Pos
		| TokenLessEqual Pos
		| TokenGreaterThan Pos
		| TokenGreaterEqual Pos
		| TokenEqual Pos
		| TokenNotEqual Pos

	----- Separadores -----
		| TokenSemicolon Pos
		| TokenPipe Pos
		| TokenQuestion Pos

	----- Entrada y Salida -----
		| TokenRead Pos
		| TokenWrite Pos


	----- Errores -------
		| TokenError String Pos
		| TokenIntError String Pos


	----- Tipos definidos ------
		| TokenInt Int Pos 
		| TokenIdentifier String Pos
		deriving (Eq,Show)

tp :: Token -> Pos
tp t = case t of

	----- Brackets ----
	( TokenLcurly p) -> p
	( TokenRcurly p) -> p
	( TokenLparenthesis p) -> p  
	( TokenRparenthesis p) -> p
	( TokenLclasp p) -> p
	( TokenRclasp p) -> p

	----- Lienzos -----
	( TokenCanvas p) -> p

	----- Constantes ----
	( TokenTrue p) -> p
	( TokenFalse p) -> p

	----- Operadores ---			 
	( TokenPlus p) -> p
	( TokenMinus p) -> p
	( TokenMult p) -> p
	( TokenDiv p) -> p
	( TokenAnd p) -> p
	( TokenOr p) -> p
	( TokenNot p) -> p
	( TokenColon p) -> p
	( TokenComma p) -> p
	( TokenRotation p) -> p
	( TokenTransposition p) -> p

	----- Relacionales -----
	( TokenLessThan p) -> p
	( TokenLessEqual p) -> p
	( TokenGreaterThan p) -> p
	( TokenGreaterEqual p) -> p
	( TokenEqual p) -> p
	( TokenNotEqual p) -> p

	----- Separadores -----
	( TokenSemicolon p) -> p
	( TokenPipe p) -> p
	( TokenQuestion p) -> p

	----- Entrada y Salida -----
	( TokenRead p) -> p
	( TokenWrite p) -> p


	----- Errores -------
	( TokenError _ p) -> p
	( TokenIntError _ p) -> p

	----- Tipos definidos ------
	(TokenInt _ p) -> p
	(TokenIdentifier _ p) -> p

printError :: Token -> IO ()
printError (TokenError s p) = do
  putStrLn $ "Error: unexpected token \"" ++ s ++ "\" " ++ show p
printError (TokenIntError s p) = do
  putStrLn $ "Error: integer out of range (-2^31 .. 2^31-1) \"" ++ s ++
    "\" " ++ show p

isTokenError :: Token -> Bool
isTokenError (TokenError    _ _) = True
isTokenError (TokenIntError _ _) = True
isTokenError _                   = False
