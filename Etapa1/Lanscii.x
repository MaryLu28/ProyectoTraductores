{
module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
import System.Environment   
import System.Directory  
import System.IO
}

%wrapper "posn"

$digit	= [0-9]
$alpha	= [a-zA-Z]
$null	= [\#]
$at 	= [\@]
$lcurly	= [\{]
$rcurly	= [\}]
$equal	= [\=]
$colon	= [\:]
$dollar = [\$]
$plus 	= [\+]
$minus 	= [\-]
$mult 	= [\*]
$caret = [\^]
$slash = [\/]
$apostrophe = [\']
$percent = [\%]
$semicolon	= [\;]
$question	= [\?]
$langular	= [\<]
$rangular	= [\>]
$exclamation = [\!]
$lparenthesis = [\(]
$rparenthesis = [\)]
$canvas = [\|\/\\\_\-]
$ascii = [\x00-\xff] # [\{\}]
$pipe = [\|]
$menorIgual = [\<=]
$mayorIgual = [\>=]
$notequal = [\/=]

@Integer = "%"
@Boolean = "!"

tokens :-

	$white+           		;     
	"--".*            		;
	"{-"$ascii*"-}"  		;
	read           			{ tok (\p s -> TokenRead p) }
	write           		{ tok (\p s -> TokenWrite p)  }
	true            		{ tok (\p s -> TokenTrue p) }
	false           		{ tok (\p s -> TokenFalse p)  }
	$digit+         		{ tok (\p s -> Int p (read s)) }
	$null 					{ tok (\p s -> TokenNull p) }
	$at 					{ tok (\p s -> TokenAt p) }
	$lcurly					{ tok (\p s -> TokenLcurly p) }
	$rcurly					{ tok (\p s -> TokenRcurly p) }
	$equal					{ tok (\p s -> TokenEqual p ) }
	$colon					{ tok (\p s -> TokenColon p ) }
	$percent				{ tok (\p s -> TokenPercent p) }
	$semicolon				{ tok (\p s -> TokenSemicolon p) }
	$question				{ tok (\p s -> TokenQuestion p) }
	$langular				{ tok (\p s -> TokenLangular p) }
	$rangular				{ tok (\p s -> TokenRangular p) }
	$slash   				{ tok (\p s -> TokenSlash p)}
	$plus					{ tok (\p s -> TokenPlus p)}
	$minus					{ tok (\p s -> TokenMinus p)}
	$mult					{ tok (\p s -> TokenMult p)}
	$caret					{ tok (\p s -> TokenCaret p)}
	$exclamation			{ tok (\p s -> TokenExclamation p) }
	$lparenthesis			{ tok (\p s -> TokenLparenthesis p) }
	$rparenthesis			{ tok (\p s -> TokenRparenthesis p) }
	$notequal				{ tok (\p s -> TokenNotEqual p)}
	$menorIgual				{ tok (\p s -> TokenMenorIgual p)}
	$mayorIgual				{ tok (\p s -> TokenMayorIgual p)}
	$langular[$canvas]$rangular	{ tok (\p s -> TokenCanvas p)}
	$alpha[$alpha $digit\_]*  { tok (\p s -> Var p s) }
	
{


tok :: (AlexPosn -> String -> Token) -> AlexPosn -> String -> Token
tok f p s = f p s

data Token = 
			TokenRead AlexPosn
			|TokenWrite AlexPosn
			|TokenTrue AlexPosn
			|TokenFalse AlexPosn
			|TokenNull AlexPosn
			|TokenAt AlexPosn
			|TokenLcurly AlexPosn
			|TokenRcurly AlexPosn
			|TokenEqual AlexPosn
			|TokenColon AlexPosn
			|TokenPercent AlexPosn
			|TokenSemicolon AlexPosn
			|TokenQuestion AlexPosn
			|TokenLangular AlexPosn
			|TokenRangular AlexPosn
			|TokenExclamation AlexPosn
			|TokenLparenthesis AlexPosn
			|TokenRparenthesis AlexPosn
			|TokenCanvas AlexPosn
			|TokenPlus AlexPosn
			|TokenCaret AlexPosn
			|TokenMinus AlexPosn
			|TokenMult AlexPosn
			|TokenSlash AlexPosn
			|TokenMayorIgual AlexPosn
			|TokenMenorIgual AlexPosn
			|TokenNotEqual AlexPosn
			|Sym AlexPosn Char 
			|Var AlexPosn String 
			|Int AlexPosn Int
			deriving (Eq,Show)

token_posn ::Token -> AlexPosn
token_posn (TokenRead p) = p
token_posn (TokenWrite p) = p
token_posn (TokenTrue p) = p
token_posn (TokenFalse p) = p
token_posn (TokenNull p) = p
token_posn (TokenAt p) = p
token_posn (TokenLcurly p) = p
token_posn (TokenRcurly p) = p
token_posn (TokenEqual p) = p
token_posn (TokenColon p) = p
token_posn (TokenPercent p) = p
token_posn (TokenSemicolon p) = p
token_posn (TokenQuestion p) = p
token_posn (TokenLangular p) = p
token_posn (TokenRangular p) = p
token_posn (TokenExclamation p) = p
token_posn (TokenLparenthesis p) = p
token_posn (TokenRparenthesis p) = p
token_posn (TokenCanvas p) = p
token_posn (TokenPlus p) = p
token_posn (TokenCaret p) = p
token_posn (TokenMinus p) = p
token_posn (TokenMult p) = p
token_posn (TokenSlash p) = p
token_posn (TokenMayorIgual p) = p
token_posn (TokenMenorIgual p) = p
token_posn (TokenNotEqual p) = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p

main :: IO ()
main = do
	[f] <- getArgs
	s <- readFile f
	print (alexScanTokens s)
}
