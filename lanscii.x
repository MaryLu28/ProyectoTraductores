{
	module Main(main) where
}

%wrapper "basic"

$digito = 0-9 -- digitos
$alfa = [a-zA-Z\_]
$simbolo = [\#\{\}\(\)\;\=\'\:\?\<\>]
$lienzos = [\ \|\/\\\_\-]
$asciiSinLlaves = [\x00-\xff] # [\{\}]

@Integer = "%"
@Boolean = "!"

tokens :-

	$white+ 					;			
	"--".* 						;
	"{-"$asciiSinLlaves*"-}" 	;
	read 						{ \s -> TokenRead	}
	write 						{ \s -> TokenWrite	}
	True						{ \s -> TokenTrue	}
	False						{ \s -> TokenFalse	}
	$digito+ 					{ \s -> Int (read s) }
	$simbolo 					{ \s -> Sym (head s) }
	$alfa[$alfa$digito\_\’]*	{ \s -> Var s }
	[\<$lienzos\>]				{ \s -> TokenCanvas	}
{
-- Each action has type :: String -> Token
-- The token type:
data Token = 
			TokenRead
			|TokenWrite
			|TokenCanvas
			|TokenTrue
			|TokenFalse 
			|Sym Char 
			|Var String 
			|Int Int
			deriving (Eq,Show)

main = do
	s <- getContents
	print (alexScanTokens s)
}
