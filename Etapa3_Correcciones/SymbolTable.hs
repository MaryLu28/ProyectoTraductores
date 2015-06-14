module SymbolTable
where

import 	qualified Data.Map as Map
import Data.Maybe 
--import Lexer
--import Derivacion
--import Parser

data Tipo = Entero | Booleano | Lienzo 
    deriving (Show) 

st = Map.empty
stack = []

insertar :: String -> Tipo -> Map.Map String Tipo -> Map.Map String Tipo
insertar s t m = Map.insert s t m

type Pila = [Map.Map String Tipo]

empilar :: Map.Map String Tipo -> Pila -> Pila
empilar m p = m:p

desempilar :: Pila -> Pila
desempilar (x:xs) = xs

desempilar' :: Pila -> Map.Map String Tipo
desempilar' (x:xs) = x

buscar :: String -> Map.Map String Tipo -> Maybe Tipo
buscar s m = Map.lookup s m

