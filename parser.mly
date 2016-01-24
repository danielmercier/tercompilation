
%token BOOLEAN_KW INT_KW STRING_KW
%token PUBLIC CLASS EXTENDS STATIC
%token TRUE FALSE
%token IF ELSE
%token FOR
%token NEW INSTANCEOF
%token NATIVE
%token NULL
%token RETURN
%token THIS
%token VOID
%token <string> IDENT
%token <int> INT
%token <string> STRING
%token COMMA SEMI DOT
%token AFFECT
%token EQ NEQ LT LE GT GE PLUS MINUS MUL DIV MOD NOT AND OR
%token INCR DECR
%token L_BRACKET R_BRACKET
%token L_PAREN R_PAREN
%token L_BRACE R_BRACE
%token MAIN
%token EOF

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%right AFFECT
%left  OR
%left  AND
%left  EQ NEQ
%left  LT LE GT GE INSTANCEOF
%left  PLUS MINUS
%left  MUL DIV MOD
%right NOT INCR DECR
%nonassoc UMINUS
%left  DOT


%start fichier
%type <unit> fichier

%%

fichier: 
    | rep_class_def class_main EOF {}
;

class_def:
    | CLASS IDENT bool_class_params bool_extends_class L_BRACE rep_decl R_BRACE {}
;

class_main:
    | PUBLIC CLASS IDENT L_BRACE PUBLIC STATIC VOID MAIN R_PAREN STRING_KW IDENT L_BRACKET R_BRACKET R_PAREN bloc {}
;

class_params:
    | LT IDENT plus_class_params GT {}
;

class_expr:
    | IDENT bool_class_expr_type {}
;

decl:
    | decl_att         {}
    | decl_const       {}
    | decl_meth        {}
    | decl_native_meth {}
;

decl_att:
    | type_expr IDENT {}
;

decl_const:
    | IDENT L_PAREN rep_decl_arg R_PAREN bloc {}
;

decl_meth:
    | type_meth IDENT L_PAREN rep_decl_arg R_PAREN bloc {}
;

decl_native_meth:
    | NATIVE type_meth IDENT L_PAREN rep_decl_arg R_PAREN SEMI {}

type_expr:
    | BOOLEAN_KW {}
    | INT_KW     {}
    | class_expr {}
;

bloc:
    | L_BRACE rep_instr_bloc R_BRACE {}
;

instr:
    | SEMI {}
    | instr_expr SEMI {}
    | type_expr IDENT bool_affect SEMI {}
    | IF L_PAREN expr R_PAREN instr %prec LOWER_THAN_ELSE {}
    | IF L_PAREN expr R_PAREN instr ELSE instr {}
    | FOR L_PAREN bool_expr SEMI bool_expr SEMI bool_expr R_PAREN instr {}
    | bloc {}
    | RETURN bool_expr SEMI {}
;

instr_expr:
    | acces AFFECT expr {}
    | appel             {}
    | incr_decr acces   {}
    | acces incr_decr   {}
    | NEW class_expr L_PAREN rep_expr R_PAREN {}
;

appel:
    | acces L_PAREN bool_expr_list R_PAREN {}
;

expr:
    | TRUE        {}
    | FALSE       {}
    | INT         {}
    | STRING      {}
    | NULL        {}
    | NOT expr    {}
    | MINUS expr %prec UMINUS    {}
    | expr INSTANCEOF class_expr {}
    | expr operateur expr        {}
    | L_PAREN cast R_PAREN expr  {}
    | instr_expr           {}
    | acces                {}
    | L_PAREN expr R_PAREN {}
;


acces:
    | IDENT             {}
    | THIS              {}
    | appel DOT IDENT   {}
    | acces DOT IDENT   {}
    | L_PAREN expr R_PAREN DOT IDENT {}
;

operateur:
    | EQ    {}
    | NEQ   {}
    | LT    {}
    | LE    {}
    | GT    {}
    | GE    {}
    | PLUS  {}
    | MINUS {}
    | MUL   {}
    | DIV   {}
    | MOD   {}
    | AND   {}
    | OR    {}
;

expr_list:
    | expr                 {}
    | expr_list COMMA expr {}
;


/* auxiliary rules */

rep_class_def:
    | {}
    | class_def rep_class_def {}

bool_class_params:
    | {}
    | class_params {}
;

bool_extends_class:
    | {}
    | EXTENDS class_expr {}
;

rep_decl:
    | {}
    | decl rep_decl {}
;

plus_class_params:
    | {}
    | COMMA IDENT plus_class_params {}
;

bool_class_expr_type:
    | {}
    | LT class_expr plus_class_expr GT {}
;

plus_class_expr:
    | {}
    | COMMA class_expr plus_class_expr {}
;

rep_decl_arg:
    | {}
    | type_expr IDENT aux_rep_decl_arg {}
;

aux_rep_decl_arg:
    | {}
    | COMMA type_expr IDENT aux_rep_decl_arg {}
;

type_meth:
    | type_expr {}
    | VOID {}
;

rep_instr_bloc:
    | {}
    | instr rep_instr_bloc {}
;

bool_affect:
    | {}
    | AFFECT expr {}
;

bool_expr:
    | {}
    | expr {}
;

incr_decr:
    | INCR {}
    | DECR {}
;

rep_expr:
    | {}
    | expr aux_rep_expr {}
;

aux_rep_expr:
    | {}
    | COMMA expr aux_rep_expr {}
;

bool_expr_list:
    | {}
    | expr_list {}
;

cast:
    | IDENT      {}
    | INT        {}
    | BOOLEAN_KW {}
;

