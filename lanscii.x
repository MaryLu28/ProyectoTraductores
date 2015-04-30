{------------------------------------------------------------------------------
	Traductores e Interpretadores
	Abril - Junio 2015
	Lanscii Etapa 1
	Integrantes: 
	Maria Lourdes Garcia Florez 10-10264
	Sahid Reyes 10-
------------------------------------------------------------------------------}

{
	--module Main (main) where
}

%wrapper "basic"

$digito = 0-9 ------------------------------> digitos
$alfa = [a-zA-Z\_] -------------------------> Letras y underscore 
$simbolo = [\#\{\}\-\(\)\/\@\;\=\<\>\|] ----> 
$asciiSinLlaves = [\x00-\xff] # [\{\}] -----> Caracteres ascii menos las llaves

tokens :-
	$white+ ;			
	"--".* ; -----------------------------------------> Comentario de linea
	"{-" $asciiSinLlaves* "-}"	----------------------> Comentario de Bloque
	$simbolo { \d -> Let }
	% { \s -> In }
	$digit+ { \s -> Int (read s) }
	[\=\+\-\*\/\(\)] { \s -> Sym (head s) }
	$alpha [$alpha $digit \_ \’]* { \s -> Var s }

{
-- Each action has type :: String -> Token
-- The token type:
data Token =
			@ 
			|In 
			|Sym Char 
			|Var String 
			|% Int
			deriving (Eq,Show)

main = do
	s <- getContents
	print (alexScanTokens s)
}
