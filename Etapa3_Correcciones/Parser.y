{module Parser
( parser
) where

import Lexer
import Data.List
import Derivacion
}

%name parser
%tokentype {Token}
%error {parseError}

%token

    ----- Brackets -----
    '{'   { TokenLcurly _ }
    '}'   { TokenRcurly _ }
    '('   { TokenLparentheses _ } 
    ')'   { TokenRparentheses _ }
    '['   { TokenLclasp _ }
    ']'   { TokenRclasp _ }
        
    ----- Lienzos -----
    "< >" { TokenCanvas _ }
    "<|>" { TokenCanvas _ }
    "<\>" { TokenCanvas _ }
    "</>" { TokenCanvas _ }
    "<_>" { TokenCanvas _ }
    "<->" { TokenCanvas _ }
    '#'   { TokenCanvas _ }

    ----- Constantes -----
    true  { TokenTrue _ }
    false { TokenFalse _ }

    ----- Tipos -----
    '%'  { TokenPercent _ }
    '@'  { TokenAt _ }
    '!'  { TokenExclamation _ }

    ----- Operadores -----           
    '+'  { TokenPlus _ }
    '-'  { TokenMinus _ }
    '*'  { TokenMult _ }
    '/'  { TokenDiv _ }
    "/\\" { TokenAnd _ }
    "\\/" { TokenOr _ }
    '^'  { TokenNot _ }
    '~'  { TokenTilde _ }
    '&'  { TokenAmpersand _ }
    '$'  { TokenRotation _ } 
    "\'" { TokenTransposition _ }

    ----- Relacionales -----
    '<'  { TokenLessThan _ }
    "<=" { TokenLessEqual _ }
    '>'  { TokenGreaterThan _ }
    ">=" { TokenGreaterEqual _ }
    '='  { TokenEqual _ }
    "/=" { TokenNotEqual _ }

    ----- Separadores -----
    ';' { TokenSemicolon _ }
    ':' { TokenColon _ }
    '|' { TokenPipe _ }
    '?' { TokenQuestion _ }
    ".."{ TokenRange _ }

    ----- Entrada y Salida -----
    read  { TokenRead _ }
    write { TokenWrite _ }

    ----- Tipos definidos -----
    var { TokenVar _ $$ }
    num { TokenInt _ $$ }

----- Precedencia Operadores -----

----- Booleanos -----
%left "\\/"
%left "/\\"
%left '^'

----- Relacionales -----
%nonassoc '<' "<=" '>' ">=" 
%nonassoc '=' "/="

----- Enteros -----
%left '+' '-'
%left '*' '/' '%'
%right NEG

----- Lienzos -----
%left '~' '&'
%right '$'
%left "\'" 

----- Condicional ------
%left ':' '?'   

%%

----- Gramatica -----

Programa: Bloque                                {Programa $1}

Bloque :'{' Declaraciones '|' Instrs '}'        {Bloque $2 $4}
       |'{' Instrs '}'                          {Bloque [] $2}

Instrs : Instr ';' Instrs                       {$1 : $3}
       | Instr                                  {[$1]}       

Instr : read var                                {Read $2}
      | write Expr                              {Write $2}
      | var '=' Expr                            {Asign $1 $3}
      | '('Expr '?' Instrs')'                   {Cond $2 $4 []}
      | '('Expr '?' Instrs ':' Instrs')'        {Cond $2 $4 $6}
      | '['Expr '|' Instrs ']'                  {While $2 $4}
      | '['Expr ".." Expr '|' Instrs']'         {For $2 $4 $6}
      | '['var ':' Expr ".." Expr '|' Instrs']' {ForIndex $2 $4 $6 $8}
      | Bloque                                  {$1}

Declaraciones
  : Declaracion Declaraciones                   {$1 : $2}
  | Declaracion                                 {[$1]}

Declaracion
  : Tipo ListVar                                {$1 $2}

Tipo
    : '%'                                       {Entero}
    | '@'                                       {Lienzo}
    | '!'                                       {Booleano}

ListVar
        : var ListVar                           {$1 : $2}
        | var                                   {[$1]}

Expr
        : Expr '+' Expr                         {Binaria Suma $1 $3}
        | Expr '-' Expr                         {Binaria Resta $1 $3}
        | Expr '*' Expr                         {Binaria Mult $1 $3}     
        | Expr '/' Expr                         {Binaria Div $1 $3}
        | Expr '%' Expr                         {Binaria Mod $1 $3}
        | Expr '<' Expr                         {Binaria Menor $1 $3}
        | Expr "<=" Expr                        {Binaria MenorIgual $1 $3}    
        | Expr '>' Expr                         {Binaria Mayor $1 $3}
        | Expr ">=" Expr                        {Binaria MayorIgual $1 $3}
        | Expr '=' Expr                         {Binaria Igual $1 $3}     
        | Expr "/=" Expr                        {Binaria Desigual $1 $3}
        | Expr "\\/" Expr                       {Binaria Or $1 $3}
        | Expr "/\\" Expr                       {Binaria And $1 $3}
        | Expr '~' Expr                         {Binaria ConcatH $1 $3}
        | Expr '&' Expr                         {Binaria ConcatV $1 $3}    
        | '('Expr')'                            {$2}
        | Expr'^'                               {Unaria Not $1}
        | '$'Expr                               {Unaria Rot $2}
        | Expr"\'"                              {Unaria Tras $1}
        | '-'Expr %prec NEG                     {Unaria Negativo $2}
        | true                                  {ConstBool True}
        | false                                 {ConstBool False}
        | num                                   {ConstEntero $1}
        | var                                   {Variable $1}
        | "< >"                                 {LienzoC " "}
        | "<|>"                                 {LienzoC "|"}
        | "<\>"                                 {LienzoC "\\"}
        | "</>"                                 {LienzoC "/"}
        | "<_>"                                 {LienzoC "_"}
        | "<->"                                 {LienzoC "-"}
        | '#'                                   {LienzoVacio}
