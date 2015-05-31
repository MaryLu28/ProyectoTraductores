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

data Programa = Programa Cuerpo deriving (Eq)

data Cuerpo
            = Declaracion


data Instr
    =
    | Asig
    | 
    | 
    deriving (Eq, Show)

Cond

Iter

List_Dec    

data Declaracion = Declaracion Tipo Var deriving (Eq, Show)


data Expr
    = Binaria     Bin   Expr  Expr
    | Unaria      Uni    Expr
    | LienzoC     [Expr]
    | BooleanoC   Bool
    | EnteroC     Int
    | StringC     String
    | Variable  Id
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

data Uni
    = Negativo   
    | Trans   
    | Not
    | Rot       


data Tipo 
    = Booleano
    | Entero
    | Lienzo
    | Strings