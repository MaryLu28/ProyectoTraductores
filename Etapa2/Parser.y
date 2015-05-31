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
Comienzo: Cuerpo							{}

Programa
		: '{' Cuerpo '}'					{$2}

Cuerpo
		: Declaracion '|' Instrs			{$1 $3}
		| Instrs							{$1}

Instrs
		: Instr ';' Instrs					{$1 $3}
		| Instr 							{$1}						
		| Programa							{$1}
Instr
		: read var							{}
		| write Expr						{}
		| var '=' Expr						{}
		| '('Cond')'						{$2}
		| '['Iter']'						{$2}

Cond
		: Expr '?' Instrs					{$1 $3}
		| Expr '?' Instrs ':' Instrs		{$1 $3 $4}

Iter
		: Expr '|' Instrs					{$1 $3}
		| Expr '..' Expr '|' Instrs			{$1 $3 $5}
		| var ':' Expr '..' Expr '|' Instrs	{}

Declaracion
		: Tipo ListVar Declaracion			{$1 $2}
		| Tipo ListVar						{}

ListVar
		: var ListVar						{$1}
		| var								{}

Expr
		: Expr '+' Expr						{Binario (Suma $1) $3}
  		| Expr '-' Expr						{Binario (Resta $1) $3}
  		| Expr '*' Expr                 	{Binario (Mult $1) $3}     
  		| Expr '/' Expr						{Binario (Div $1) $3}
  		| Expr '%' Expr						{Binario (Mod $1) $3}
  		| Expr '<' Expr						{Binario (Menor $1) $3}
  		| Expr '<=' Expr             		{Binario (MenorIgual $1) $3}    
  		| Expr '>' Expr						{Binario (Mayor $1) $3}
   		| Expr '>=' Expr					{Binario (MayorIgual $1) $3}
  		| Expr '=' Expr                   	{Binario (Igual $1) $3}     
  		| Expr '/=' Expr					{Binario (Desigual $1) $3}
  		| Expr '\/' Expr					{Binario (And $1) $3}
  		| Expr '/\\' Expr					{Binario (Or $1) $3}
  		| Expr '~' Expr					    {Binario (ConcatH $1) $3}
  		| Expr '&' Expr                     {Binario (ConcatV $1) $3}    
  		| '('Expr')'						{Unario $2}
  		| Expr'^'							{Unario $1}
  		| '$'Expr							{Unario $2}
  		| Expr'\''							{Unario $1}
  		| '-'Expr %prec NEG					{Unario $2}
  		| true 								{}
  		| false 							{}
  		| num 								{}
  		| var 								{}
		| '< >'   							{}
		| '<|>' 							{}
		| '<\>' 							{}
		| '</>' 							{}
		| '<_>'								{}
		| '<->' 							{}
		| '#'  								{}

Tipo
	: '%'									{Entero}
	| '@'									{Linezo}
	| '!'									{Booleano}
