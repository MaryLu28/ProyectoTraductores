module Derivacion
(Comienzo(..) 
,Programa(..)
, Instr(..)
, Cuerpo(..)
, Instr(..)
, Declaracion(..)
, Tipo(..)
, Expr(..)
--, Binaria(..)
--, Unaria(..)
, Bin(..)
, Uni(..)
, Var(..)
) where

import Data.List
import Lexer

data Comienzo = Comienzo Programa deriving (Eq, Show)

data Programa = Programa Cuerpo deriving (Eq, Show)

data Var = Var String deriving (Eq, Show)

data Cuerpo = Cuerpo (Maybe Declaracion) Instr deriving (Eq, Show)

data Declaracion = Tipo [Var] deriving (Eq, Show)

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
	| LienzoC     Expr
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