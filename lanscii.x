{
	--module Main (main) where
}

%wrapper "basic"

$digito = 0-9 -- digitos
$alfa = [a-zA-Z\_]
$simbolo = [\#\{\}\-\(\)\/\@\;\=\<\>\|]
$asciiSinLlaves = [\x00-\xff] # [\{\}]

tokens :-
	$white+ ;			
	"--".* ;
	"{-" $asciiSinLlaves* "-}"	
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
