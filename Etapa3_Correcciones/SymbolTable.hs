module SymbolTable
where

import 	qualified Data.Map as M
import Data.Maybe 
--import Lexer
import Derivacion
--import Parser

data Tipo = Entero | Booleano | Lienzo 
    deriving (Show) 

type Mapa = M.Map String Tipo

type Pila = [Mapa]

st = M.empty :: Mapa
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

buscar :: String -> Mapa -> Maybe Tipo
buscar s m = M.lookup s m

class Process a where
	process :: a -> Pila -> Pila
 
--instance Process Programa where
--	process (Programa ins) ini = process ins (st:ini)