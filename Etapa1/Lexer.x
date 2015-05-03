-----------------------------------------------------------------------------
--    Traductores e Interpretadores
  --  Abril - Junio 2015
    --Lanscii Etapa 1 Lexer
 --   Integrantes: 
   -- Maria Lourdes Garcia Florez 10-10264
   -- Sahid Reyes 10-10603
-----------------------------------------------------------------------------

{
    module Lexer
    ( 
        lexer, alexScanTokens
    ) 
    where
    import Tokens (Pos(..), Token(..), isTokenError, printError)
}

%wrapper "posn"

$digit  = 0-9  -- Digitos
$alpha  = [a-zA-Z]   --Caracteres Alfabeticos
$ascii  = [\x00-\xff] --Todos los caracteres de ascii

tokens :-

    ----- Espacios y Comentarios -----
    $white+                 ;
    "--".*                  ;
    "{-"$ascii*"-}"         ;

    ----- Braquets -----
    "{"                         {tok TokenLcurly}
    "}"                         {tok TokenRcurly}
    "("                         {tok TokenLparenthesis}
    ")"                         {tok TokenRparenthesis}
    "["                         {tok TokenLclasp}
    "]"                         {tok TokenRclasp}

    ----- Lienzos -----
    "<"[\/\\\|\-\_\ ]">"   {tok TokenCanvas}
    "#"                         {tok TokenCanvas}

    ----- Constantes -----
    true                        {tok TokenTrue}
    false                       {tok TokenFalse}
    $digit+                     {LexInt}

    ----- Variables ------
    $alpha[$alpha $digit\_]*    {toq TokenIdentifier id}

    ----- Tipos -----
    "%"                         {tok TokenPercent}
    "@"                         {tok TokenAt}
    "!"                         {tok TokenExclamation}

    ----- Operadores -----
    "+"                         {tok TokenPlus}
    "-"                         {tok TokenMinus}
    "*"                         {tok TokenMult}
    "/"                         {tok TokenDiv}
    "/\\"                       {tok TokenAnd}
    "\/"                        {tok TokenOr}
    "^"                         {tok TokenNot}
    ":"                         {tok TokenColon}
    ","                         {tok TokenComma}
    "$"                         {tok TokenRotation}
    "'"                         {tok TokenTransposition}

    ----- Relacionales -----
    "<"                         {tok TokenLessThan}
    "<="                        {tok TokenLessEqual}
    ">"                         {tok TokenGreaterThan}
    ">="                        {tok TokenGreaterEqual}
    "="                         {tok TokenEqual}
    "/="                        {tok TokenNotEqual}

    ----- Separadores -----
    ";"                         {tok TokenSemicolon}
    "|"                         {tok TokenPipe}
    "?"                         {tok TokenQuestion}

    ----- Entrada y Salida -----
    read                        {tok TokenRead}
    write                       {tok TokenWrite}

    ----- Error -----
    .                           {toq TokenError id}

{
   ---------------------------------------------------------------------
    
    -----------------------------------------------------------------------
    toPos :: AlexPosn -> Pos
    toPos (AlexPn _ line column) = Pos line column

    ----------------------------------------------------------------------

    ------------------------------------------------------------------------
    tok :: (Pos -> Token) -> AlexPosn -> String -> Token
    tok f p _ = f (toPos p)

    toq :: (a -> Pos -> Token) -> (String -> a) -> AlexPosn -> String -> Token
    toq f g p s = f (g s) (toPos p)

    lexInt :: AlexPosn -> String -> Token
    lexInt p s
        n < 2^31    = TokenInt      n (toPos p)
        | n < 2^31   = TokenInt      n (toPos p)
        | otherwise = TokenIntError s (toPos p)
        where n = (read s :: (Num a, Read a) => a)

    ------------------------------------------------------------------------- 
    
    ---------------------------------------------------------------------
    lexer :: String -> String -> IO ()
    lexer text name = do
        putStrLn $ "Lexer (" ++ name ++ "):\n"
        let toks = alexScanTokens text

        if any isTokenError toks
            then mapM_ printError $ filter isTokenError toks
            else mapM_ print toks
        putStrLn ""
}