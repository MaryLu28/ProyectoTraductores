{
module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
import System.Environment   
import System.Directory  
import System.IO
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
    "{"                         {tok (\p s -> TokenLcurly p)}
    "}"                         {tok (\p s -> TokenRcurly p)}
    "("                         {tok (\p s -> TokenLparenthesis p)}
    ")"                         {tok (\p s -> TokenRparenthesis p)}
    "["                         {tok (\p s -> TokenLclasp p)}
    "]"                         {tok (\p s -> TokenRclasp p)}

    ----- Lienzos -----
    "<"[\/\\\|\-\_\ ]">"   		{tok (\p s -> TokenCanvas p)}
    "#"                         {tok (\p s -> TokenCanvas p)}

    ----- Constantes -----
    true                        {tok (\p s -> TokenTrue p)}
    false                       {tok (\p s -> TokenFalse p)}
    (\-?)$digit+                {tok (\p s -> Int p (read s))}

    ----- Variables ------
    $alpha[$alpha $digit\_]*    {tok (\p s -> Var p s)}

    ----- Tipos -----
    "%"                         {tok (\p s -> TokenPercent p)}
    "@"                         {tok (\p s -> TokenAt p)}
    "!"                         {tok (\p s -> TokenExclamation p)}

    ----- Operadores -----
    "+"                         {tok (\p s -> TokenPlus p)}
    "-"                         {tok (\p s -> TokenMinus p)}
    "*"                         {tok (\p s -> TokenMult p)}
    "/"                         {tok (\p s -> TokenDiv p)}
    "/\\"                       {tok (\p s -> TokenAnd p)}
    "\/"                        {tok (\p s -> TokenOr p)}
    "^"                         {tok (\p s -> TokenNot p)}
    ":"                         {tok (\p s -> TokenColon p)}
    ","                         {tok (\p s -> TokenComma p)}
    "$"                         {tok (\p s -> TokenRotation p)}
    "'"                         {tok (\p s -> TokenTransposition p)}

    ----- Relacionales -----
    "<"                         {tok (\p s -> TokenLessThan p)}
    "<="                        {tok (\p s -> TokenLessEqual p)}
    ">"                         {tok (\p s -> TokenGreaterThan p)}
    ">="                        {tok (\p s -> TokenGreaterEqual p)}
    "="                         {tok (\p s -> TokenEqual p)}
    "/="                        {tok (\p s -> TokenNotEqual p)}

    ----- Separadores -----
    ";"                         {tok (\p s -> TokenSemicolon p)}
    "|"                         {tok (\p s -> TokenPipe p)}
    "?"                         {tok (\p s -> TokenQuestion p)}
    
    ----- Entrada y Salida -----
    read           				{ tok (\p s -> TokenRead p)}
	write           			{ tok (\p s -> TokenWrite p)}


	
{

tok :: (AlexPosn -> String -> Token) -> AlexPosn -> String -> Token
tok f p s = f p s

data Token = 
		----- Brackets ----
			  TokenLcurly AlexPosn
			| TokenRcurly AlexPosn
			| TokenLparenthesis AlexPosn 
			| TokenRparenthesis AlexPosn
			| TokenLclasp AlexPosn
			| TokenRclasp AlexPosn
			
		----- Lienzos -----
			| TokenCanvas AlexPosn

		----- Constantes ----
			| TokenTrue AlexPosn
			| TokenFalse AlexPosn

 	   
    	 ----- Tipos -----
			|TokenPercent AlexPosn
			|TokenAt AlexPosn
			|TokenExclamation AlexPosn

		----- Operadores ---			 
			| TokenPlus AlexPosn
			| TokenMinus AlexPosn
			| TokenMult AlexPosn
			| TokenDiv AlexPosn
			| TokenAnd AlexPosn
			| TokenOr AlexPosn
			| TokenNot AlexPosn
			| TokenColon AlexPosn
			| TokenComma AlexPosn
			| TokenRotation AlexPosn 
			| TokenTransposition AlexPosn

		----- Relacionales -----
			| TokenLessThan AlexPosn
			| TokenLessEqual AlexPosn
			| TokenGreaterThan AlexPosn
			| TokenGreaterEqual AlexPosn
			| TokenEqual AlexPosn
			| TokenNotEqual AlexPosn

		----- Separadores -----
			| TokenSemicolon AlexPosn
			| TokenPipe AlexPosn
			| TokenQuestion AlexPosn


		----- Entrada y Salida -----
			| TokenRead AlexPosn
			| TokenWrite AlexPosn

		----- Tipos definidos ------
			|Var AlexPosn String 
			|Int AlexPosn Int
			deriving (Eq,Show)

token_posn ::Token -> AlexPosn
token_posn (TokenLcurly p) = p
token_posn (TokenRcurly p) = p
token_posn (TokenLparenthesis p) = p 
token_posn (TokenRparenthesis p) = p
token_posn (TokenLclasp p) = p
token_posn (TokenRclasp p) = p
token_posn (TokenCanvas p) = p
token_posn (TokenTrue p) = p
token_posn (TokenFalse p) = p
token_posn (TokenPlus p) = p
token_posn (TokenMinus p) = p
token_posn (TokenMult p) = p
token_posn (TokenDiv p) = p
token_posn (TokenAnd p) = p
token_posn (TokenOr p) = p
token_posn (TokenNot p) = p
token_posn (TokenColon p) = p
token_posn (TokenComma p) = p
token_posn (TokenRotation p) = p 
token_posn (TokenTransposition p) = p
token_posn (TokenLessThan p) = p
token_posn (TokenLessEqual p) = p
token_posn (TokenGreaterThan p) = p
token_posn (TokenGreaterEqual p) = p
token_posn (TokenEqual p) = p
token_posn (TokenNotEqual p) = p
token_posn (TokenSemicolon p) = p
token_posn (TokenPipe p) = p
token_posn (TokenQuestion p) = p
token_posn (TokenRead p) = p
token_posn (TokenWrite p) = p
token_posn (Var p _) = p
token_posn (Int p _) = p

main :: IO ()
main = do
	[f] <- getArgs
	s <- readFile f
	print (alexScanTokens s)
}