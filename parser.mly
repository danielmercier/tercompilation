%token <int> INT
%token PLUS MINUS MULT DIV
%start expr
%type <int> expr
%%
expr:  
    INT { $1 }
  | expr PLUS expr { $1 + $3 }
  | expr MINUS expr { $1 - $3 }
  | expr MULT expr { $1 * $3 }
  | expr DIV expr { $1 / $3 }
;
