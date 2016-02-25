%{
    open Error
    open Ast
    open Lexing

    let current_pos () =
        (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())

    let mk_node elem =
        {value=elem; pos=current_pos ()}
%}

%token <string> IDENT STRING
%token <Int32.t> CONST
%token PLUS UMINUS MINUS MULT DIV MOD
%token INC DEC 
%token EQ NEQ
%token NOT OR AND 
%token LE LT GE GT 
%token RPAR LPAR 
%token LBRA RBRA
%token LEMB REMB
%token AFFECT
%token DOT 
%token COM
%token SEMICOLON
%token IF ELSE FOR
%token BOOLEAN TRUE FALSE
%token CLASS
%token MAIN
%token EXTENDS
%token INSTANCEOF
%token INT
%token NATIVE
%token NEW
%token NULL
%token PUBLIC
%token RETURN
%token STATIC
%token THIS
%token VOID
%token EOF
%token CAST
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

%%
prog:
    rep_class_def class_main EOF    { ($1, $2) }
  | error                           { error (Syntax_error None) ( current_pos () ) }
;

class_def:
    CLASS IDENT opt_class_params opt_extends_class_expr LEMB rep_decl REMB { ($2, $3, $4, $6) }
;

/*Ajout d'un non terminal pour que la position de l'erreur soit plus précise*/
tstring:
    IDENT {
        if($1 <> "String") then
            error (Syntax_error
                    (Some ("literal \"String\" expected but \"" ^ $1 ^ "\" found")))
                  (current_pos ())
    }
;

/*Ajout d'un non terminal pour que la position de l'erreur soit plus précise*/
classname:
    IDENT {
        mk_node $1
    }
;

class_main:
    PUBLIC CLASS classname LEMB PUBLIC STATIC VOID MAIN LPAR tstring IDENT LBRA RBRA RPAR bloc REMB {
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
    acces AFFECT expr                  { mk_node (Affect($1, $3)) }
  | appel                              { $1 }
  | INC acces                          { mk_node (PreIncr $2) }
  | DEC acces                          { mk_node (PreDecr $2) }
  | acces INC                          { mk_node (PostIncr $1) }
  | acces DEC                          { mk_node (PostDecr $1) }
  | NEW class_expr LPAR rep_expr RPAR  { mk_node (New($2, $4)) }
;

appel:
    acces LPAR opt_expr_list RPAR { mk_node (Appel($1, $3)) }
;

expr:
    TRUE                          { mk_node (Const (Cbool true)) }
  | FALSE                         { mk_node (Const (Cbool false)) }
  | CONST                         { mk_node (Const (Cint $1)) }
  | STRING                        { mk_node (Const (Cstring $1)) }
  | NULL                          { mk_node (Const Cnull) }
  | NOT expr                      { mk_node (Unop (Unot, $2)) }
  /* %prec Permet a MINUS de ce comporter avec les règle de UMINUS */
  | MINUS expr %prec UMINUS       { mk_node (Unop (Uminus, $2)) } 
  | expr PLUS expr                { mk_node (Intbinop(Add, $1, $3)) }
  | expr MINUS expr               { mk_node (Intbinop(Sub, $1, $3)) }
  | expr MULT expr                { mk_node (Intbinop(Mult, $1, $3)) }
  | expr DIV expr                 { mk_node (Intbinop(Div, $1, $3)) }
  | expr MOD expr                 { mk_node (Intbinop(Mod, $1, $3)) }
  | expr AND expr                 { mk_node (Boolbinop(And, $1, $3)) }
  | expr OR expr                  { mk_node (Boolbinop(Or, $1, $3)) }
  | expr EQ expr                  { mk_node (Bineq(Eq, $1, $3)) }
  | expr NEQ expr                 { mk_node (Bineq(Neq, $1, $3)) }
  | expr LT expr                  { mk_node (Intbincmp(Lt, $1, $3)) }
  | expr LE expr                  { mk_node (Intbincmp(Le, $1, $3)) }
  | expr GT expr                  { mk_node (Intbincmp(Gt, $1, $3)) }
  | expr GE expr                  { mk_node (Intbincmp(Ge, $1, $3)) }
  | expr INSTANCEOF class_expr    { mk_node (Instanceof($1, $3)) }
  | cast expr   %prec CAST        { mk_node (Cast($1, $2)) }
  | instr_expr                    { $1 }
  | acces                         { $1 }
  | LPAR expr RPAR                { $2 }
;

acces:
    IDENT %prec PREC_ACCES_IDENT  { mk_node (Ident $1) }
  | THIS                          { mk_node (This) }
  | appel DOT IDENT               { mk_node (DotAcces($1, $3)) }
  | acces DOT IDENT               { mk_node (DotAcces($1, $3)) }
  | LPAR expr RPAR DOT IDENT      { mk_node (DotAcces($2, $5)) }
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
