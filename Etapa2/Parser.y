{module Parser
( parser
) where

import Lexer
import Data.List
}

%name parser
%tokentype {Token}
%error {parseError}

%token

	----- Brackets -----
	'{'	  { TokenLcurly _ }
	'}'	  { TokenRcurly _ }
	'('	  { TokenLparentheses _ } 
	')'	  { TokenRparentheses _ }
	'['	  { TokenLclasp _ }
	']'	  { TokenRclasp _ }
		
	----- Lienzos -----
	'< >' { TokenCanvas _ }
	'<|>' { TokenCanvas _ }
	'<\>' { TokenCanvas _ }
	'</>' { TokenCanvas _ }
	'<_>' { TokenCanvas _ }
	'<->' { TokenCanvas _ }
	'#'   { TokenCanvas _ }

	----- Constantes -----
	true  { TokenTrue _ }
	false { TokenFalse _ }

	----- Tipos -----
	'%'	 { TokenPercent _ }
	'@'	 { TokenAt _ }
	'!'	 { TokenExclamation _ }

	----- Operadores -----			 
	'+'	 { TokenPlus _ }
	'-'	 { TokenMinus _ }
	'*'	 { TokenMult _ }
	'/'	 { TokenDiv _ }
	'/\\' { TokenAnd _ }
	'\/' { TokenOr _ }
	'^'	 { TokenNot _ }
	'~'	 { TokenTilde _ }
	'&'	 { TokenAmpersand _ }
	'$'	 { TokenRotation _ } 
	'\'' 	 { TokenTransposition _ }

	----- Relacionales -----
	'<'	 { TokenLessThan _ }
	'<=' { TokenLessEqual _ }
	'>'	 { TokenGreaterThan _ }
	'>=' { TokenGreaterEqual _ }
	'='	 { TokenEqual _ }
	'/=' { TokenNotEqual _ }

	----- Separadores -----
	';'	{ TokenSemicolon _ }
	':'	{ TokenColon _ }
	'|'	{ TokenPipe _ }
	'?'	{ TokenQuestion _ }
	'..'{ TokenRange _ }

	----- Entrada y Salida -----
	read  { TokenRead _ }
	write { TokenWrite _ }

	----- Tipos definidos -----
	var	{ TokenVar _ _ }
	num	{ TokenInt _ _ }

----- Precedencia Operadores -----
-----FALTA--------------------------------------------
%left '+' '-' '*' '/' '~' '%' '&' '\'' ':' '?' '^' '/\\' '\/'

%right  '$' NEG 

%nonassoc '<' '<=' '>' '>=' '=' '/='

%%

----- Gramatica -----

--- FALTA-----------------------------------------------
Comienzo: Programa							{Comienzo $1}

Programa
		: '{' Cuerpo '}'					{Programa $2}

Cuerpo
		: Declaracion '|' Instrs			{Cuerpo $1 $3}
		| Instrs							{Cuerpo $1}

Instrs
		: Instr ';' Instrs					{$1 : $3}
		| Instr 							{$1}						
		| Programa							{$1}
Instr
		: read var							{Read Variable (extraerV $2)}
		| write Expr						{Write $2}
		| var '=' Expr						{Asign Variable (extraerV $1) $3}
		| '('Cond')'						{$2}
		| '['Iter']'						{$2}

Cond
		: Expr '?' Instrs					{Cond $1 $3}
		| Expr '?' Instrs ':' Instrs		{Cond $1 $3 $4}

Iter
		: Expr '|' Instrs					{$1 $3}
		| Expr '..' Expr '|' Instrs			{$1 $3 $5}
		| var ':' Expr '..' Expr '|' Instrs	{}

Declaracion
		: Tipo ListVar Declaracion			{Declaracion ($1 $2) : $3}
		| Tipo ListVar						{$1 $2}

ListVar
		: var ListVar						{Variable (extraerV $1) : $2}
		| var								{[Variable (extraerV $1)]}

Expr
		: Expr '+' Expr						{Binaria Suma $1 $3}
  		| Expr '-' Expr						{Binaria Resta $1 $3}
  		| Expr '*' Expr                 	{Binaria Mult $1 $3}     
  		| Expr '/' Expr						{Binaria Div $1 $3}
  		| Expr '%' Expr						{Binaria Mod $1 $3}
  		| Expr '<' Expr						{Binaria Menor $1 $3}
  		| Expr '<=' Expr             		{Binaria MenorIgual $1 $3}    
  		| Expr '>' Expr						{Binaria Mayor $1 $3}
   		| Expr '>=' Expr					{Binaria MayorIgual $1 $3}
  		| Expr '=' Expr                   	{Binaria Igual $1 $3}     
  		| Expr '/=' Expr					{Binaria Desigual $1) $3}
  		| Expr '\/' Expr					{Binaria Or $1 $3}
  		| Expr '/\\' Expr					{Binaria And $1 $3}
  		| Expr '~' Expr					    {Binaria ConcatH $1 $3}
  		| Expr '&' Expr                     {Binaria ConcatV $1 $3}    
  		| '('Expr')'						{$2}
  		| Expr'^'							{Unaria Not $1}
  		| '$'Expr							{Unaria Rot $2}
  		| Expr'\''							{Unaria Trans $1}
  		| '-'Expr %prec NEG					{Unaria Negativo $2}
  		| true 								{True}
  		| false 							{False}
  		| num 								{EnteroC (extraerI $1)}
  		| var 								{Variable (extraerV $1)}
		| '< >'   							{LienzoC (extraerC$1)}
		| '<|>' 							{LienzoC (extraerC$1)}
		| '<\>' 							{LienzoC (extraerC$1)}
		| '</>' 							{LienzoC (extraerC$1)}
		| '<_>'								{LienzoC (extraerC$1)}
		| '<->' 							{LienzoC (extraerC$1)}
		| '#'  								{LienzoC (extraerC$1)}

Tipo
	: '%'									{Entero}
	| '@'									{Lienzo}
	| '!'									{Booleano}

{
extraerV :: Token -> String
extraer (TokenVar s _)  = s

extraerI' :: Token -> Int
extraerI (TokenInt n _) = n

extraerC :: Token -> String
extraerC (TokenCanvas l _) = l
}