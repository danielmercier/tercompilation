%{
%}

%token <string> IDENT STRING
%token <int> CONST
%token RPAR LPAR PLUS MINUS MULT DIV INC DEC MOD EQ AFFECT 
%token NOT OR AND NEQ LE LT GE GT DOT TYPESTRING LBRA RBRA
%token BOOLEAN CLASS ELSE LEMB REMB SEMICOLON COM MAIN
%token EXTENDS FALSE FOR IF INSTANCEOF INT NATIVE NEW NULL
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

%start file
%type <unit> file class_def class_main rep_class_def opt_class_params class_params
%type <unit> opt_extends_class_expr rep_decl opt_class_params decl
%type <unit> decl_att decl_const decl_meth decl_native_meth type rep_type_ident
%type <unit> type_or_void bloc instr opt_affect_expr opt_expr rep_type_ident_com rep_expr
%type <unit> rep_expr_com

%%
file:
    rep_class_def class_main EOF    { }
;

class_def: 
    CLASS IDENT opt_class_params opt_extends_class_expr LEMB rep_decl REMB { }
;

class_main:
    PUBLIC CLASS IDENT LEMB PUBLIC STATIC VOID MAIN LPAR TYPESTRING IDENT LBRA RBRA RPAR bloc REMB { }
;

class_params:
    LT rep1_ident GT { }

class_expr:
    IDENT                       { }
  | IDENT LT rep1_class_expr GT { }

decl:
    decl_att          { }
  | decl_const        { }
  | decl_meth         { }
  | decl_native_meth  { }
;

decl_att:
    type IDENT SEMICOLON    { }
;

decl_const:
    IDENT LPAR rep_type_ident RPAR bloc   { }
;

decl_meth:
    type_or_void IDENT LPAR rep_type_ident RPAR bloc    { }
;

decl_native_meth:
    NATIVE type_or_void IDENT LPAR rep_type_ident RPAR SEMICOLON    { }
;

type:
    BOOLEAN     { }
  | INT         { }
  | class_expr  { }
;

bloc:
    LEMB rep_instr REMB { }
;

instr:
    SEMICOLON                                                          { }
  | instr_expr SEMICOLON                                                { }  
  | type IDENT opt_affect_expr SEMICOLON                               { }       
  | IF LPAR expr RPAR instr                                            { }           
  | IF LPAR expr RPAR instr ELSE instr                                 { }         
  | FOR LPAR opt_expr SEMICOLON opt_expr SEMICOLON opt_expr RPAR instr { }                
  | bloc                                                               { }       
  | RETURN opt_expr SEMICOLON                                          { }         

instr_expr:
    acces AFFECT expr                  { }   
  | appel                              { }         
  | INC acces                          { }           
  | MINUS acces                        { }           
  | acces INC                          { }         
  | acces MINUS                        { }       
  | NEW class_expr LPAR rep_expr RPAR  { }             

appel:
    acces LPAR opt_expr_list RPAR { }

expr:
    TRUE                    { }
  | FALSE                   { }
  | CONST                   { }
  | STRING                  { }
  | NULL                    { }
  | NOT expr                { }
  | MINUS expr              { }
  | expr PLUS expr          { }
  | expr MINUS expr         { }
  | expr MULT expr          { }
  | expr DIV expr           { }
  | expr MOD expr           { }
  | expr AND expr           { }
  | expr OR expr            { }
  | expr EQ expr            { }
  | expr NEQ expr           { }
  | expr LT expr            { }
  | expr LE expr            { }
  | expr GT expr            { }
  | expr GE expr            { }
  | expr INSTANCEOF expr    { }
  | LPAR IDENT RPAR expr    { }
  | LPAR INT RPAR expr      { }
  | LPAR BOOLEAN RPAR expr  { }
  | instr_expr              { }
  | acces                   { }
  | LPAR expr RPAR          { }

acces:
    IDENT                    { }
  | THIS                     { }  
  | appel DOT IDENT          { }     
  | acces DOT IDENT          { }     
  | LPAR expr RPAR DOT IDENT { } 

expr_list:
    expr                { }
  | expr_list COM expr  { }

type_or_void:
    type  { }
  | VOID  { }
;

/* Tout les non terminaux optionnel */
opt_class_params:
    /* empty */   { }
  | class_params  { }

opt_extends_class_expr:
    /* empty */        { }
  | EXTENDS class_expr { }

opt_expr_list:
    /* empty */        { }
  | expr_list          { }

opt_affect_expr:
    /* empty */  { }
  | AFFECT expr  { }

opt_expr:
    /* empty */  { }
  | expr         { }

/************************************/

/* Tout les non terminaux répétable */
rep_type_ident:
    /* empty */        { }
  | rep_type_ident_com { }
;

rep_type_ident_com:
    type IDENT                        { }
  | type IDENT COM rep_type_ident_com { }

rep_decl:
    /* empty */   { }
  | decl rep_decl { }
  
rep_class_def:
    /* empty */               { }
  | class_def rep_class_def   { }

rep_instr:
    /* empty */     { }
  | rep_instr instr { }

rep_expr:
    /* empty */   { }
  | rep_expr_com  { }

rep_expr_com:
    expr                  { }
  | expr COM rep_expr_com { }

rep1_ident:
    IDENT                { }
  | IDENT COM rep1_ident { }

rep1_class_expr:
    class_expr                      { }
  | class_expr COM rep1_class_expr  { }
/************************************/
  

%%
