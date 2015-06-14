module SymbolChecking
where

import qualified SymbolTable as ST
import Derivacion

newtype Resultado = Resultado (ST.Pila, [String])

-- Clase para procesar el AST
class Process a where
	process :: a -> Resultado -> Resultado

-- Como procesar una lista de cosas
instance (Process a) => Process [a] where
	process [] res = res
	process (x:xs) res = let nuevoRes = process x res 
		in process xs nuevoRes	
			
-- Como procesar un programa
instance Process Programa where
	process (Programa ins) res@(Resultado (st, errs)) =
		process ins (Resultado (ST.empilar ST.mapaNuevo st, errs))

-- Como procesar una instruccion
instance Process Instr where
	-- Como procesar una asignacion
	process (Asign s expr) res@(Resultado (st, errs)) =
		case ST.buscarCompleto s st of
			Just _ -> process expr res
			Nothing -> process expr (Resultado (st, extra:errs))
		where
			extra = "Variable " ++ show s ++ "no existe en este alcance."  
	-- Como procesar un Read
	process (Read s) res@(Resultado (st, errs)) =
		case ST.buscarCompleto s st of
			Just t -> case t of
				Int -> res
				Bool -> res
				Canvas -> Resultado (st, extra2:errs) 
			Nothing -> Resultado (st, extra:errs)
		where
			extra = "Variable " ++ show s ++ " no existe en este alcance."
			extra2 = "Instruccion Read espera tipo Int o Bool pero obtuvo tipo Canvas." 
	-- Como procesar Write
	--process (Write expr) res@(Resultado (st, errs)) =
	--	case ST.buscarCompleto s st of
	--		Just t -> case t of
	--			ST.Entero -> Resultado (st, extra2:errs)
	--			ST.Booleano -> Resultado (st, extra2:errs)
	--			ST.Lienzo -> res
	--		Nothing -> process expr (Resultado (st, extra:errs))
	--	where
	--		extra = "Variable " ++ show s ++ " no existe en este alcance."
	--		extra2 = "Variable " ++ show s ++ " no es de tipo valido para Write."

--	process (Cond expr [ins1] [ins2]) = 
--	process (While expr [ins]) =
--	process (For expr1 expr2 [ins]) =
--	process (ForIndex s expr1 expr2 [ins]) =
--	process (Bloque [decl] [ins]) res@(Resultado (st, errs))=

--instance Process exprr where
--	process (Binaria bin expr1 expr2) =
--	process (Unaria uni expr) =
--	process (LienzoC s) =
--	process (ConstBool bool) =
--	process (ConstEntero int) =
--	process (Variable s) =
--	process (LienzoVacio) =

--instance Process Declaracion where
----	 = 