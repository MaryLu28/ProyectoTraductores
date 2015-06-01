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

data Comienzo = Programa deriving (Eq)

data Programa = Cuerpo deriving (Eq)

data Cuerpo =  Declaracion Instr deriving (Eq)

data Declaracion = Declaracion Tipo Var deriving (Eq, Show)

data Instr
	= Asign    Var Expr
	| Bloque   [Declaracion] [Instr]
	| Read     Var
	| Write    [Expr]
	| If       Expr Instr (Maybe Instr)
	deriving (Eq, Show)

data Expr
	= Binaria     Bin   Expr  Expr
	| Unaria      Uni    Expr
	| LienzoC     Lienzo
	| BooleanoC   Bool
	| EnteroC     Int
	| StringC     String
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