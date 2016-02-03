%{
  open Error
  open Ast

  let current_pos () =
    (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())
%}

%token <string> IDENT STRING
%token <Int32.t> CONST
%token RPAR LPAR PLUS MINUS MULT DIV INC DEC MOD EQ AFFECT
%token NOT OR AND NEQ LE LT GE GT DOT LBRA RBRA
%token BOOLEAN CLASS ELSE LEMB REMB SEMICOLON COM MAIN
%token EXTENDS FALSE FOR IF INSTANCEOF INT NATIVE NEW NULL
%token PUBLIC RETURN STATIC THIS TRUE VOID EOF UMINUS CAST
%token PREC_IF PREC_ACCES_IDENT

%nonassoc PREC_IF
%nonassoc ELSE

%nonassoc PREC_ACCES_IDENT
%nonassoc RPAR    /* Pour le cast */

%nonassoc IDENT_CLASS_EXPR /* Precedence moins forte que LT */

%right  AFFECT
%left   OR
%left   AND
%left   EQ NEQ
%left   LT LE GT GE INSTANCEOF /* On utilise LT dans class_expr aussi */
%left   PLUS MINUS
%left   MULT DIV MOD
%right  NOT INC DEC CAST
%nonassoc UMINUS
%left   DOT

/* Pbm avec le moins unaire, résolu avec l'option %prec et UMINUS */

%start prog
%type <Ast.prog> prog
%type <Ast.class_def> class_def
%type <Ast.class_main> class_main
%type <Ast.class_params> class_params
%type <Ast.class_expr> class_expr
%type <Ast.decl> decl decl_att decl_const decl_meth decl_native_meth
%type <Ast.type_> type_
%type <Ast.bloc> bloc rep_instr
%type <Ast.instr> instr
%type <Ast.expr> expr instr_expr acces appel
%type <Ast.expr list> expr_list opt_expr_list rep_expr rep_expr_com
%type <Ast.type_> cast
%type <Ast.class_params> class_params
%type <Ast.class_expr option> opt_extends_class_expr
%type <Ast.expr option> opt_affect_expr opt_expr
%type <Ast.args> rep_type_ident rep_type_ident_com
%type <Ast.decl list> rep_decl
%type <Ast.class_def list> rep_class_def
%type <Ast.ident list> rep1_ident
%type <Ast.class_expr list> rep1_class_expr

%%
prog:
    rep_class_def class_main EOF    { ($1, $2) }
  | error                           { error (Syntax_error None) ( current_pos () ) }
;

class_def:
    CLASS IDENT opt_class_params opt_extends_class_expr LEMB rep_decl REMB { ($2, $3, $4, $6) }
;

tstring:
    IDENT { if($1 <> "String")
            then error
                   (Syntax_error (Some ("literal \"String\" expected but \"" ^ $1 ^ "\" found")))
                   (current_pos ())
    }
;

class_main:
    PUBLIC CLASS IDENT LEMB PUBLIC STATIC VOID MAIN LPAR tstring IDENT LBRA RBRA RPAR bloc REMB {
      ($3, $11, $15)
    }
;

class_params:
    LT rep1_ident GT { $2 }

class_expr:
    IDENT  %prec IDENT_CLASS_EXPR  { CIdent($1, []) }
  | IDENT LT rep1_class_expr GT    { CIdent($1, $3) }

decl:
    decl_att          { $1 }
  | decl_const        { $1 }
  | decl_meth         { $1 }
  | decl_native_meth  { $1 }
;

decl_att:
    type_ IDENT SEMICOLON    { DeclAtt($1, $2) }
;

decl_const:
    IDENT LPAR rep_type_ident RPAR bloc   { DeclConst($1, $3, $5) }
;

decl_meth:
    VOID IDENT LPAR rep_type_ident RPAR bloc    { DeclMeth(Tvoid, $2, $4, $6) }
  | type_ IDENT LPAR rep_type_ident RPAR bloc   { DeclMeth($1, $2, $4, $6) }
;

decl_native_meth:
    NATIVE VOID IDENT LPAR rep_type_ident RPAR SEMICOLON     { DeclNativeMeth(Tvoid, $3, $5) }
  | NATIVE type_ IDENT LPAR rep_type_ident RPAR SEMICOLON    { DeclNativeMeth($2, $3, $5) }
;

type_:
    BOOLEAN     { Tbool }
  | INT         { Tint }
  | class_expr  { Tident $1 }
;

bloc:
    LEMB rep_instr REMB { $2 }
;

instr:
    SEMICOLON                                                          { Nothing }
  | instr_expr SEMICOLON                                               { Iexpr $1 }
  | type_ IDENT opt_affect_expr SEMICOLON                              { Declaration($1, $2, $3) }
  | IF LPAR expr RPAR instr   %prec PREC_IF                            { If($3, $5) }
  | IF LPAR expr RPAR instr ELSE instr                                 { IfElse($3, $5, $7) }
  | FOR LPAR opt_expr SEMICOLON opt_expr SEMICOLON opt_expr RPAR instr { For($3, $5, $7, $9) }
  | bloc                                                               { Bloc $1 }
  | RETURN opt_expr SEMICOLON                                          { Return $2 }


instr_expr:
    acces AFFECT expr                  { Affect($1, $3) }
  | appel                              { $1 }
  | INC acces                          { PreIncr $2 }
  | DEC acces                          { PreDecr $2 }
  | acces INC                          { PostIncr $1 }
  | acces DEC                          { PostDecr $1 }
  | NEW class_expr LPAR rep_expr RPAR  { New($2, $4) }
;

appel:
    acces LPAR opt_expr_list RPAR { Appel($1, $3) }
;

expr:
    TRUE                          { Const (Cbool true) }
  | FALSE                         { Const (Cbool false) }
  | CONST                         { Const (Cint $1) }
  | STRING                        { Const (Cstring $1) }
  | NULL                          { Const Cnull }
  | NOT expr                      { Unop (Unot, $2) }
  | MINUS expr %prec UMINUS       { Unop (Uminus, $2) } /* Permet a MINUS de ce comporter avec les règle de UMINUS */
  | expr PLUS expr                { Binop(Add, $1, $3) }
  | expr MINUS expr               { Binop(Sub, $1, $3) }
  | expr MULT expr                { Binop(Mult, $1, $3) }
  | expr DIV expr                 { Binop(Div, $1, $3) }
  | expr MOD expr                 { Binop(Mod, $1, $3) }
  | expr AND expr                 { Binop(And, $1, $3) }
  | expr OR expr                  { Binop(Or, $1, $3) }
  | expr EQ expr                  { Binop(Eq, $1, $3) }
  | expr NEQ expr                 { Binop(Neq, $1, $3) }
  | expr LT expr                  { Binop(Lt, $1, $3) }
  | expr LE expr                  { Binop(Le, $1, $3) }
  | expr GT expr                  { Binop(Gt, $1, $3) }
  | expr GE expr                  { Binop(Ge, $1, $3) }
  | expr INSTANCEOF class_expr    { Instanceof($1, $3) }
  | cast expr   %prec CAST        { Cast($1, $2) }
  | instr_expr                    { $1 }
  | acces                         { $1 }
  | LPAR expr RPAR                { $2 }
;

acces:
    IDENT %prec PREC_ACCES_IDENT  { Ident $1 }
  | THIS                          { This }
  | appel DOT IDENT               { DotAcces($1, $3) }
  | acces DOT IDENT               { DotAcces($1, $3) }
  | LPAR expr RPAR DOT IDENT      { DotAcces($2, $5) }
;

expr_list:
    expr                { [$1] }
  | expr_list COM expr  { $3::$1 }
;

cast:
    LPAR IDENT RPAR     { Tident (CIdent($2, [])) }
  | LPAR INT RPAR       { Tint }
  | LPAR BOOLEAN RPAR   { Tbool }

/* Tout les non terminaux optionnel */
opt_class_params:
    /* empty */   { [] }
  | class_params  { $1 }
;

opt_extends_class_expr:
    /* empty */        { None }
  | EXTENDS class_expr { Some $2 }
;

opt_expr_list:
    /* empty */        { [] }
  | expr_list          { $1 }
;

opt_affect_expr:
    /* empty */  { None }
  | AFFECT expr  { Some $2 }
;

opt_expr:
    /* empty */  { None }
  | expr         { Some $1 }
;

/************************************/

/* Tout les non terminaux répétable */
rep_type_ident:
    /* empty */        { [] }
  | rep_type_ident_com { $1 }
;

rep_type_ident_com:
    type_ IDENT                        { [($1, $2)] }
  | rep_type_ident_com COM type_ IDENT { ($3, $4)::$1 }
;

rep_decl:
    /* empty */   { [] }
  | rep_decl decl { $2::$1 }
;

rep_class_def:
    /* empty */               { [] }
  | rep_class_def class_def   { $2::$1 }
;

rep_instr:
    /* empty */     { [] }
  | rep_instr instr { $2::$1 }
;

rep_expr:
    /* empty */   { [] }
  | rep_expr_com  { $1 }
;

rep_expr_com:
    expr                  { [$1] }
  | rep_expr_com COM expr { $3::$1 }
;

rep1_ident:
    IDENT                { [$1] }
  | rep1_ident COM IDENT { $3::$1 }
;

rep1_class_expr:
    class_expr                      { [$1] }
  | rep1_class_expr COM class_expr  { $3::$1 }
;
/************************************/
%%
