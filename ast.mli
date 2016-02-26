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
  | Unop of unop * expr node

  (* Opérations séparée pour le typage *)
  | Intbinop of intbinop * expr node * expr node
  | Bineq of bineq * expr node * expr node
  | Intbincmp of intbincmp * expr node * expr node
  | Boolbinop of boolbinop * expr node * expr node
  (*************************************)

  | Instanceof of expr node * class_expr
  | Cast of type_ * expr node

  (* instr_expr dans la grammaire *)
  | Affect of expr node * expr node
  | PostIncr of expr node
  | PreIncr of expr node
  | PostDecr of expr node
  | PreDecr of expr node
  | New of class_expr * (expr node) list
  (********************************)

  (* acces dans la grammaire *)
  | Ident of ident
  | This
  | DotAcces of expr node * ident
  (***************************)

  (* appel dans la grammaire *)
  | Appel of expr node * (expr node) list
  (***************************)

type bloc = instr list

and instr =
  | Nothing
  | Iexpr of expr node
  | Declaration of type_ * ident * (expr node) option
  | If of expr node * instr
  | IfElse of expr node * instr * instr
  | For of (expr node) option *
           (expr node) option *
           (expr node) option *
           instr
  | Bloc of bloc
  | Return of (expr node) option

type args = (type_ * ident) list

type decl =
  | DeclAtt of type_ * ident
  | DeclConst of ident * args * bloc
  | DeclMeth of type_ * ident * args * bloc
  | DeclNativeMeth of type_ * ident * args

type class_params = ident list
type class_def = ident * class_params * class_expr option * decl list
type class_main = ident node * ident * bloc

type prog = class_def list * class_main
