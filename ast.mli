type position = Lexing.position * Lexing.position

type ident = string
type 'a node = { value: 'a; pos: position }

type const =
  | Cbool of bool
  | Cint of Int32.t
  | Cstring of string
  | Cnull

type class_expr = 
  | CIdent of ident * (class_expr) list

type type_ =
  | Tbool
  | Tint
  | Tvoid
  | Tident of class_expr

type unop =
  | Uminus
  | Unot

type intbinop =
  | Add | Sub | Mult | Div | Mod

type bineq = 
  | Eq | Neq 

type intbincmp =
  | Lt | Le | Gt | Ge

type boolbinop =
  | And | Or
  
type expr =
  | Const of const
  | Unop of unop * expr

  (* Opérations séparée pour le typage *)
  | Intbinop of intbinop * expr * expr
  | Bineq of bineq * expr * expr
  | Intbincmp of intbincmp * expr * expr
  | Boolbinop of boolbinop * expr * expr
  (*************************************)

  | Instanceof of expr * class_expr
  | Cast of type_ * expr

  (* instr_expr dans la grammaire *)
  | Affect of expr * expr
  | PostIncr of expr
  | PreIncr of expr
  | PostDecr of expr
  | PreDecr of expr
  | New of class_expr * (expr) list
  (********************************)

  (* acces dans la grammaire *)
  | Ident of ident
  | This
  | DotAcces of expr * ident
  (***************************)

  (* appel dans la grammaire *)
  | Appel of expr * (expr) list
  (***************************)

type bloc = instr list

and instr =
  | Nothing
  | Iexpr of expr
  | Declaration of type_ * ident * expr option
  | If of expr * instr
  | IfElse of expr * instr * instr
  | For of expr option * expr option * expr option * instr
  | Bloc of bloc
  | Return of expr option

type args = (type_ * ident) list

type decl =
  | DeclAtt of type_ * ident
  | DeclConst of ident * args * bloc
  | DeclMeth of type_ * ident * args * bloc
  | DeclNativeMeth of type_ * ident * args

type class_params = ident list
type class_def = ident * class_params * class_expr option * decl list
type class_main = ident * ident * bloc

type prog = class_def list * class_main
