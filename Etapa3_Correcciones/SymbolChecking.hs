import qualified SymbolTable as ST
import Derivacion

newtype Resultado = Resultado (ST.Pila, [String])

-- Clase para procesar el AST
class Process a where
	process :: a -> Resultado -> Resultado

-- Como procesar un programa
instance Process Programa where
	process (Programa ins) res@(Resultado (st, errs)) =
		process ins (Resultado (ST.empilar ST.mapaNuevo st, errs))

-- Como procesar una instruccion
instance Process Instr where
	-- Como procesar una asignacion
	process (Asign s exp) res@(Resultado (st, errs)) =
		case ST.buscarCompleto s st of
			Just _ -> process exp res
			Nothing -> process exp (Resultado (st, extra:errs))
		where
			extra = "Variable " ++ show s ++ "no existe en este alcance."  
	-- Como procesar un Read
	process (Read s) res@(Resultado (st, errs)) =
		case ST.buscarCompleto s st of
			Just t -> case t of
				ST.Entero -> res
				ST.Booleano -> res
				ST.Lienzo -> Resultado (st, extra2:errs) 
			Nothing -> process exp (Resultado (st, extra:errs))
		where
			extra = "Variable " ++ show s ++ " no existe en este alcance."
			extra2 = "Variable " ++ show s ++ " no es de tipo valido para Read."
	-- Como procesar Write
	--process (Write exp) res@(Resultado (st, errs)) =
	--	case ST.buscarCompleto s st of
	--		Just t -> case t of
	--			ST.Entero -> Resultado (st, extra2:errs)
	--			ST.Booleano -> Resultado (st, extra2:errs)
	--			ST.Lienzo -> res
	--		Nothing -> process exp (Resultado (st, extra:errs))
	--	where
	--		extra = "Variable " ++ show s ++ " no existe en este alcance."
	--		extra2 = "Variable " ++ show s ++ " no es de tipo valido para Write."
--	process (Cond exp [ins1] [ins2]) = 
--	process (While exp [ins]) =
--	process (For exp1 exp2 [ins]) =
--	process (ForIndex s exp1 exp2 [ins]) =
--	process (Bloque [decl] [ins]) =

--instance Process Expr where
--	process (Binaria bin exp1 exp2) =
--	process (Unaria uni exp) =
--	process (LienzoC s) =
--	process (ConstBool bool) =
--	process (ConstEntero int) =
--	process (Variable s) =
--	process (LienzoVacio) =

--instance Process Declaracion where
----	 = 