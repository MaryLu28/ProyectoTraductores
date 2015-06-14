module SymbolTable
( Pila
, Tipo(..)
, mapaNuevo
, insertar
, empilar
, desempilar
, desempilar'
, buscar
, buscarCompleto
) where

import 	qualified Data.Map as M
import Data.Maybe 
--import Lexer
import Derivacion
--import Parser

data Tipo = Entero | Booleano | Lienzo 
    deriving (Show) 

type Mapa = M.Map String Tipo

type Pila = [Mapa]

mapaNuevo = M.empty :: Mapa
stack = []

-- Insertar en el mapa actual
insertar :: String -> Tipo -> Pila -> Pila
insertar s t (m:ms) = (M.insert s t m) : ms

empilar :: Mapa -> Pila -> Pila
empilar m p = m:p

-- Desempilar retornando la pila restante
desempilar :: Pila -> Pila
desempilar (x:xs) = xs

-- Desempila retornando lo desempilado
desempilar' :: Pila -> Mapa
desempilar' (x:xs) = x

buscar :: String -> Pila -> Maybe Tipo
buscar s (x:xs) = M.lookup s x

buscarCompleto :: String -> Pila -> Maybe Tipo
buscarCompleto _ [] = Nothing
buscarCompleto s pila@(x:xs) = case buscar s pila of
	Just t -> Just t
	Nothing -> buscarCompleto s xs