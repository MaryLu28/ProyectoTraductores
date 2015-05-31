{''
module Parser
( parser
, parsr
) where

import Lexer
}

%name parser
%tokentype {Token}
%error {parseError}

%token
	----- Brackets ----
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

	----- Constantes ----
	true  { TokenTrue _ }
	false { TokenFalse _ }

	------ Tipos -----
	'%'	 { TokenPercent _ }
	'@'	 { TokenAt _ }
	'!'	 { TokenExclamation _ }

	----- Operadores ---			 
	'+'	 { TokenPlus _ }
	'-'	 { TokenMinus _ }
	'*'	 { TokenMult _ }
	'/'	 { TokenDiv _ }
	'/\\' { TokenAnd _ }
	'\/' { TokenOr _ }
	'^'	 { TokenNot _ }
	'~'	 { TokenTilde _ }
	'&'	 { TokenAmpersand _ }
	','	 { TokenComma _ }
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

	----- Tipos definidos ------
	str	{ TokenVar _ _ }
	num	{ TokenInt _ _ }

---Precedencia Operadores---
-----FALTA--------------------------------------------
%left '+' '-' '*' '/' '~' '%' '&' '\'' ':' '?'

%right '^' '$' '-'

%nonassoc '<' '<=' '>' '>='

%%

---Gramatica-----
--- FALTA-----------------------------------------------
Programa: '{' Cuerpo '}'					{Programa $2}

Cuerpo: 
		Declaracion '|' Instr				{}
		| Declaracion '|' Programa			{}
		| Declaracion '|' Instr Programa	{}

Instr:
		read str							{}
		| write Expr						{}
		| str '=' Expr						{}
		| Instr ';' Instr					{}
		| '('Cond')'						{}
		| '['Iter']'						{}

Cond:
		Expr '?' Instr						{$1 $3}
		| Expr '?' Instr ':' Instr			{$1 $3 $4}

Iter:
		Expr '|' Instr						{}
		| Expr '..' Expr '|' Instr			{}
		| str ':' Expr'..'Expr '|' Instr	{}

Declaracion:								
		Tipo List_Dec						{$1 $2}
		| Tipo str							{}

List_Dec: 
		List_Dec							{}
		| str								{}

Expr:
		Expr '+'   Expr						{}
  		| Expr '-'   Expr					{}
  		| Expr '*'   Expr                   {}     
  		| Expr '/'   Expr					{}
  		| Expr '%'   Expr					{}
  		| Expr '<'   Expr					{}
  		| Expr '<='   Expr             		{}    
  		| Expr '>'   Expr					{}
   		| Expr '>='   Expr					{}
  		| Expr '='   Expr                   {}     
  		| Expr '/='   Expr					{}
  		| Expr '\/'  Expr					{}
  		| Expr '/\\'  Expr					{}
  		| Expr '~' Expr					    {}
  		| Expr '&' Expr                     {}    
  		| Expr '*' Expr                     {}     
  		| Expr '/' Expr                     {}    
  		| '('Expr')'						{}
  		| '^'Expr							{}
  		| '$'Expr							{}
  		| Expr '\''							{}
  		| true 								{}
  		| false 							{}
  		| num 								{}
  		| '-'num							{}
  		| str 								{}
		| '< >'   							{}
		| '<|>' 							{}
		| '<\>' 							{}
		| '</>' 							{}
		| '<_>'								{}
		| '<->' 							{}
		| '#'  								{}

Tipo:
	'%'										{}
	| '@'									{}
	| '!'									{}