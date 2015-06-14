module Derivacion
( Programa(..) 
, Instr(..)
, Declaracion(..)
, Expr(..)
, Bin(..)
, Uni(..)
, parseError
) where

import Data.List
import Lexer

data Programa = Programa Instr deriving (Eq, Show)


data Declaracion
	= Entero [String]
	| Lienzo [String]
	| Booleano [String]
	deriving (Eq, Show)

data Instr
	= Asign    String Expr
	| Read     String
	| Write    Expr
	| Cond     Expr [Instr] [Instr]
	| While    Expr [Instr]
	| For             Expr Expr [Instr]
	| ForIndex String Expr Expr [Instr]
	| Bloque [Declaracion] [Instr]
	deriving (Eq, Show)

data Expr
	= Binaria     Bin   Expr  Expr
	| Unaria      Uni    Expr
	| LienzoC     String
	| ConstBool   Bool
	| ConstEntero Int
	| Variable 	  String
	| LienzoVacio	
	deriving (Eq, Show)

data Bin
	= Suma
	| Resta
	| Mult
	| Div
	| Mod
	| ConcatH
	| ConcatV
	| And 
	| Or
	| Menor
	| MenorIgual
	| Mayor
	| MayorIgual
	| Igual
	| Desigual
	deriving (Eq, Show)

data Uni
	= Negativo   
	| Tras   
	| Not
	| Rot       
	deriving (Eq, Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"