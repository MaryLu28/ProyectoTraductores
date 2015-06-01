module Derivacion
( Programa(..)
, Instr(..)
, Condicion(..)
, Instruccion(..)
, Lista_Dec(..)
, Declaracion(..)
, Tipo(..)
, Expr(..)
, Binario(..)
, Unario(..)
) where

import Data.List
import Lexer

data Comienzo = Comienzo Programa deriving (Eq, Show)

data Programa = Programa Cuerpo deriving (Eq, Show)

data Cuerpo = Declaracion (Maybe Tipo [Var]) Instr deriving (Eq, Show)
	
data Instr
	= Asign    Var Expr
	| Read     Var
	| Write    [Expr]
	| Cond     Expr Instr (Maybe Instr)
--	| Iter	   Expr Instr 
	deriving (Eq, Show)

data Expr
	= Binaria     Bin   Expr  Expr
	| Unaria      Uni    Expr
	| LienzoC     Lienzo
	| BooleanoC   Bool
	| EnteroC     Int
	| Variable 	  Var
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
	deriving (Eq, Show)

data Uni
	= Negativo   
	| Trans   
	| Not
	| Rot       
	deriving (Eq, Show)

data Tipo 
	= Booleano
	| Entero
	| Lienzo
	| Strings
	deriving (Eq, Show)

data Var = Var String deriving (Eq)