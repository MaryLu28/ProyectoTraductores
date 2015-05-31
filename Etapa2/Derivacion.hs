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

Programa

Cuerpo

data Instr
	=
	| Asig
	| 
	deriving (Eq, Show)

Cond

Iter

Declaracion								

List_Dec

data Expr
    = Binaria     Bin   Expr  Expr
    | Unaria      Uni    Expr
    | LienzoC     Lienzo
    | BooleanoC   Bool
    | EnteroC     Int
    | Variable    Var
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
    | Mayor
    | Igual
    | Desigual
    | MenorIgual
    | MayorIgual
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
