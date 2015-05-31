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
    = Binaria     Bin    Expr  Expr
    | Unaria      Uni    Expr
    | CanvasC     [Expr]
    | BooleanoC   Bool
    | EnteroC     Int
    | StringC     String
    | Variable    Id
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
    | Canvas
    | Strings