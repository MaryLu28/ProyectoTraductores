{
	module Main(main) where
}

%wrapper "basic"

$digito = 0-9 -- digitos
$alfa = [a-zA-Z\_]
$simbolo = [\#\{\}\(\)\;\=\:\?\<\>]
$lienzos = [\ \|\/\_\-]
$asciiSinLlaves = [\x00-\xff] # [\{\}]
@Integer = "%"
@Boolean = "!"
@lienzos = "<"$lienzos">" 
tokens :-
	$white+ ;			
	"--".* ;
	"{-" $asciiSinLlaves* "-}"	
	read { \s -> Sym (head s) }
	write { \s -> Sym (head s) }
	$digito+ { \s -> Int (read s) }
	$simbolo { \s -> Sym (head s) }
	$alfa[$alfa$digito\_\’]* { \s -> Var s }

{
-- Each action has type :: String -> Token
-- The token type:
data Token =  
			Sym Char 
			|Var String 
			|Int Int
			deriving (Eq,Show)

main = do
	s <- getContents
	print (alexScanTokens s)
}
