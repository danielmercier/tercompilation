%{
%}

%token <string> IDENT STRING
%token <int> INT
%token RPAR LPAR PLUS MINUS MULT DIV INC DEC MOD EQ AFFECT 
%token NOT OR AND NEQ LE LT GE GT DOT
%token BOOLEAN CLASS ELSE
%token EXTENDS FALSE FOR IF INSTANCEOF TINT NATIVE NEW NULL
%token PUBLIC RETURN STATIC THIS TRUE VOID EOF

%right  AFFECT
%left   OR
%left   AND
%left   EQ NEQ
%left   LT LE GT GE INSTANCEOF
%left   PLUS MINUS
%left   MULT DIV MOD
%right  NOT INC DEC CAST
%left   DOT
/* Pbm avec le moins unaire, résolu dans la grammaire directement */

%start expr
%type <int> expr

%%
expr: 
      INT { $1 }
    | LPAR expr RPAR         { $2 }
    | expr PLUS expr    { $1 / $3 }
    | expr MINUS expr    { $1 / $3 }
    | expr MULT expr    { $1 / $3 }
    | expr DIV expr    { $1 / $3 }
    | expr DIV expr    { $1 / $3 }
    | MINUS expr             { -$2 }
;
%%
