module Derivacion
( Programa(..) 
, Instr(..)
, Declaracion(..)
, Expr(..)
, Bin(..)
, Uni(..)
, Tipo(..)
, parseError
) where

import Data.List
import Lexer

data Programa = Programa Instr deriving (Eq, Show)

data Tipo 
	= Int
	| Canvas
	| Bool

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
	| Unaria      Uni   Expr
	| LienzoC     String
	| ConstBool   Bool
	| ConstEntero Int
	| Variable 	  String
	| LienzoVacio	
	deriving (Eq, Show)

data Bin
	-- Operaciones sobre enteros
	= Suma
	| Resta
	| Mult
	| Div
	| Mod
	-- Operaciones sobre lienzos
	| ConcatH
	| ConcatV
	-- Operaciones sobre booleanos
	| And 
	| Or
	-- Operaciones comparativas
	| Menor
	| MenorIgual
	| Mayor
	| MayorIgual
	| Igual
	| Desigual
	deriving (Eq, Show)

data Uni
	-- Operaciones sobre enteros
	= Negativo   
	-- Operaciones sobre lienzos
	| Tras   
	| Rot
	-- Operaciones sobre booleanos       
	| Not
	deriving (Eq, Show)


parseError :: [Token] -> a
parseError _ = error "Parse error"

agregarTipos :: Tipo -> [String] -> [(Tipo, String)]
agregarTipos t lis = map (mezclar t) lis
	where
		mezclar t str = (t, str)

--tipoExp :: Expr -> Tipo
--tipoExp (Binaria)