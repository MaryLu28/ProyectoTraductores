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
    process (Write expr) res@(Resultado (st, errs)) =
      case tipoExpr expr res of
            Just t -> case t of
                Int -> Resultado (st, extra:errs)
                Bool -> Resultado (st, extra:errs)
                Canvas -> process expr res
            Nothing -> process expr res
      where
            extra = "Instruccion Write espera tipo Canvas pero obtuvo tipo " ++
                show (tipoExpr expr res) ++ "."
    -- Como procesar un condicional
    process (Cond expr ins1 ins2) res@(Resultado (st, errs)) = -- esta bien?
        case tipoExpr expr res of
            Just t -> case t of
                Int -> Resultado (st, extra:errs)
                Canvas -> Resultado (st, extra:errs)
                Bool -> process ins2 . process ins1 . process expr $ res 
            Nothing -> process expr res
        where
            extra = "Condicional espera expresion tipo Bool pero obtuvo tipo " ++ 
                show (tipoExpr expr res) ++ "."
    -- Como procesar un while
    process (While expr ins) res@(Resultado (st, errs)) =
        case tipoExpr expr res of
            Just t -> case t of
                Int -> Resultado (st, extra:errs)
                Canvas -> Resultado (st, extra:errs)
                Bool -> process ins . process expr $ res ---------- esta bien?
            Nothing -> process expr res
        where
            extra = "Interaccion espera expresion tipo Bool pero obtuvo tipo " ++ 
                show (tipoExpr expr res) ++ "."
    -- Como procesar un for
    --process (For expr1 expr2 ins) res@(Resultado (st, errs)) = ----- esta bien?
    --    case tipoExpr expr1 res of
    --        Nothing -> process expr1 res
    --        Just t1 -> case t1 of
    --            Canvas -> Resultado (st, extra1:errs)
    --            Bool -> Resultado (st, extra1:errs)
    --            Int -> case tipoExpr expr2 res of
    --                Just Nothing -> process expr1 res
    --                Just t2 -> case t2 of
    --                    Canvas -> Resultado (st, extra2:errs)
    --                    Bool -> Resultado (st, extra2:errs)
    --                    Int -> process ins res
    --                where
    --                    extra2 = "Interaccion espera expresion tipo Bool pero obtuvo tipo " ++ 
    --                        show (tipoExpr expr2 res) ++ "."
    --            where
    --                extra1 = "Interaccion espera expresion tipo Bool pero obtuvo tipo " ++ 
    --                    show (tipoExpr expr1 res) ++ "."
    -- Como procesar un ForIndex
    -- process (ForIndex s expr1 expr2 ins) res@(Resultado (st, errs)) =
        --case tipoExpr expr1 res of
    --        Nothing -> process expr1 res
    --        Just t1 -> case t1 of
    --            Canvas -> Resultado (st, extra1:errs)
    --            Bool -> Resultado (st, extra1:errs)
    --            Int -> case tipoExpr expr2 res of
    --                Just Nothing -> process expr1 res
    --                Just t2 -> case t2 of
    --                    Canvas -> Resultado (st, extra2:errs)
    --                    Bool -> Resultado (st, extra2:errs)
    --                    Int -> process ins res
    --                where
    --                    extra2 = "Interaccion espera expresion tipo Bool pero obtuvo tipo " ++ 
    --                        show (tipoExpr expr2 res) ++ "."
    --            where
    --                extra1 = "Interaccion espera expresion tipo Bool pero obtuvo tipo " ++ 
    --                    show (tipoExpr expr1 res) ++ "."
    -- Como procesar un bloque
    process (Bloque decl ins) res@(Resultado (st, errs)) =
        process ins . process decl $ res

-- Como procesar una expresion
instance Process Expr where
    -- Como procesar operaciones binarias
    process (Binaria bin expr1 expr2) res@(Resultado (st, errs)) =
        case compatibleBinExp bin expr1 expr2 res of
            Just _ -> process expr2 . process expr1 $ res
            Nothing -> Resultado (st, extra:errs)
        where 
            extra = "Operador " ++ show bin ++ " no funciona con tipo " ++
                show (tipoExpr expr1 res) ++ " y " ++ show (tipoExpr expr1 res) ++ "."
    -- Como procesar operaciones unaria
    process (Unaria uni expr) res@(Resultado (st, errs)) =
        case compatibleUnExp uni expr res of
            Just _ -> process expr res
            Nothing -> Resultado (st, extra:errs)
        where 
            extra = "Operador " ++ show uni ++ " no funciona con tipo " ++
                show (tipoExpr expr res) ++ "."
    -- Como procesar variables
    process (Variable s) res@(Resultado (st, errs)) =
        case ST.buscarCompleto s st of
            Just _ -> res
            Nothing -> Resultado (st, extra:errs)
        where
            extra = "Variable " ++ show s ++ "no existe en este alcance."
    -- Como procesar constantes
    process _ res = res


-- Como procesar decalraciones
instance Process Declaracion where
    -- Como procesar declaraciones de enteros
    process (Entero lis) res =
        process (agregarTipos Int lis) res
    -- Como procesar declaraciones de lienzos
    process (Lienzo lis) res =
        process (agregarTipos Canvas lis) res
    -- Como procesar declaraciones de booleanos
    process (Booleano lis) res =
        process (agregarTipos Bool lis) res

-- Como procesar tupas de las variables con su tipo
instance Process (Tipo, String) where
    process (t, s) res@(Resultado (st, errs)) = 
        case ST.buscar s st of 
            Just _ -> Resultado (st, extra:errs)
            Nothing -> Resultado (ST.insertar s t st, errs) 
        where
            extra = "Variable " ++ show s ++ " ya fue definida en el alcance."

-- funcion principal que chequea el tipo de la expresion
tipoExpr :: Expr -> Resultado -> Maybe Tipo
tipoExpr (Binaria op e1 e2) res = compatibleBinExp op e1 e2 res
tipoExpr (Unaria op e) res = compatibleUnExp op e res
tipoExpr (ConstEntero _) _ = Just Int
tipoExpr (ConstBool _) _ = Just Bool
tipoExpr (LienzoC _) _ = Just Canvas
----------- tipoExpr (Variable (Identifier iden)) res@(Resultado (st, errs)) =

-- chequea el tipo de las expresiones binarias
compatibleBinExp :: Bin -> Expr -> Expr -> Resultado -> Maybe Tipo
compatibleBinExp op e1 e2 res = case (tipoExpr e1 res, op, tipoExpr e2 res) of
    -- Logicos
    (Just Bool, Or, Just Bool) -> Just Bool
    (Just Bool, And, Just Bool) -> Just Bool
    -- Aritmetios
    (Just Int, Suma, Just Int) -> Just Int
    (Just Int, Resta, Just Int) -> Just Int
    (Just Int, Mult, Just Int) -> Just Int
    (Just Int, Div, Just Int) -> Just Int
    (Just Int, Mod, Just Int) -> Just Int
    -- Relacionales
    (Just Int, Menor, Just Int) -> Just Bool
    (Just Int, Mayor, Just Int) -> Just Bool
    (Just Int, Igual, Just Int) -> Just Bool
    (Just Int, Desigual, Just Int) -> Just Bool
    (Just Int, MenorIgual, Just Int) -> Just Bool
    (Just Int, MayorIgual, Just Int) -> Just Bool
    -- Liensos
    (Just Canvas, ConcatH, Just Canvas) -> Just Canvas
    (Just Canvas, ConcatV, Just Canvas) -> Just Canvas
    _ -> Nothing

-- chequea el tipo de las expresiones unarias
compatibleUnExp :: Uni -> Expr -> Resultado -> Maybe Tipo
compatibleUnExp op e res = case (op, tipoExpr e res) of
    -- Aritmeticas
    (Negativo, Just Int) -> Just Int
    -- lienzos
    (Rot, Just Canvas) -> Just Canvas
    (Tras, Just Canvas) -> Just Canvas
    -- Booleano
    (Not, Just Bool) -> Just Bool
    _ -> Nothing
