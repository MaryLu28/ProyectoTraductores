{
  module Main(main) where
}

%wrapper "basic"

$digit	= [0-9]
$alpha	= [a-zA-Z]
$null	= [\#]
$at 	= [\@]
$lcurly	= [\{]
$rcurly	= [\}]
$equal	= [\=]
$colon	= [\:]
$percent = [\%]
$semicolon	= [\;]
$question	= [\?]
$langular	= [\<]
$rangular	= [\>]
$exclamation = [\!]
$lparenthesis = [\(]
$rparenthesis = [\)]
$canvas = [\ \|\/\\\_\-]
$ascii = [\x00-\xff] # [\{\}]

@Integer = "%"
@Boolean = "!"

tokens :-

	$white+           		;     
	"--".*            		;
	"{-"$ascii*"-}"  		;
	read           			{ \s -> TokenRead }
	write           		{ \s -> TokenWrite  }
	True            		{ \s -> TokenTrue }
	False           		{ \s -> TokenFalse  }
	$digit+         		{ \s -> Int (read s) }
	$null 					{ \s -> TokenNull }
	$at 					{ \s -> TokenAt }
	$lcurly					{ \s -> TokenLcurly }
	$rcurly					{ \s -> TokenRcurly }
	$equal					{ \s -> TokenEqual }
	$colon					{ \s -> TokenColon }
	$percent				{ \s -> TokenPercent }
	$semicolon				{ \s -> TokenSemicolon }
	$question				{ \s -> TokenQuestion }
	$langular				{ \s -> TokenLangular }
	$rangular				{ \s -> TokenRangular }
	$exclamation			{ \s -> TokenExclamation }
	$lparenthesis			{ \s -> TokenLparenthesis }
	$rparenthesis			{ \s -> TokenRparenthesis }
	$langular$canvas$rangular	{ \s -> TokenCanvas }
	$alpha[$alpha $digit\_\’]*  { \s -> Var s }
	
{
data Token = 
			TokenRead
			|TokenWrite
			|TokenTrue
			|TokenFalse
			|TokenNull
			|TokenAt
			|TokenLcurly
			|TokenRcurly
			|TokenEqual
			|TokenColon
			|TokenPercent
			|TokenSemicolon
			|TokenQuestion
			|TokenLangular
			|TokenRangular
			|TokenExclamation
			|TokenLparenthesis
			|TokenRparenthesis
			|TokenCanvas
			|Sym Char 
			|Var String 
			|Int Int
			deriving (Eq,Show)

main = do
	s <- getContents
	print (alexScanTokens s)
}