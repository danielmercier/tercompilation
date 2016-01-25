%{
  module Env = Map.Make(String)
  let env = ref Env.empty
%}

%token <int> CONST
%token <string> IDENT
%token PLUS MINUS MULT DIV MOD EXP LPAR RPAR AFFECT
%token EOF EOL

%left PLUS MINUS
%left MULT DIV MOD
%right EXP

%start file
%type <unit> file
%type <int> expr
%type <unit> affect

%%
file:
    /* empty */     { }
  | file expr EOF   { print_int $2; print_newline() }
  | file affect EOF { }
  | file expr EOL   { print_int $2; print_newline() }
  | file affect EOL { }
;

expr:
    CONST               { $1 }
  | IDENT               { Env.find $1 !env }
  | expr PLUS expr      { $1 + $3 }
  | expr MINUS expr     { $1 - $3 }
  | expr MULT expr      { $1 * $3 }
  | expr DIV expr       { $1 / $3 }
  | expr MOD expr       { $1 mod $3 }
  | expr EXP expr       { int_of_float((float_of_int $1) ** (float_of_int $3)) }
;

affect:
    IDENT AFFECT expr   { env := Env.add $1 $3 !env; print_string ($1 ^ ": "); print_int $3; print_newline() }
