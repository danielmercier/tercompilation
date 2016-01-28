type position = Lexing.position * Lexing.position

type ident = string
type 'a node = { value: 'a; pos: position }

(* Valeur optionnel, existe ou pas *)
type 'a option = 
  | None
  | Some of 'a

type const =
  | Cbool of bool
  | Cint of int
  | Cstring of string
  | Cnull

type class_expr =
  | Tclass of ident * (class_expr node) list

type type_ =
  | Tbool
  | Tint
  | Tvoid
  | Tident of class_expr node

type unop =
  | Uminus
  | Unot

type binop =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  
type expr =
  | Const of const
  | Unop of unop * expr node
  | Binop of binop * expr node * expr node
  | Instanceof of expr node * class_expr node
  | Cast of type_ * expr node

  (* instr_expr dans la grammaire *)
  | Affect of expr node * expr node
  | PostIncr of expr node
  | PreIncr of expr node
  | PostDecr of expr node
  | PreDecr of expr node
  | New of class_expr node * (expr node) list
  (********************************)

  (* acces dans la grammaire *)
  | Ident of ident node
  | This
  | DotAcces of expr node * ident node
  (***************************)

  (* appel dans la grammaire *)
  | Appel of expr node * (expr node) list
  (***************************)

type bloc = instr list

type instr =
  | Nothing
  | Iexpr of expr node
  | Declaration of type_ * ident * expr option
  | If of expr * instr
  | IfElse of expr * instr * instr
  | For of expr option * expr option * expr option * instr
  | Bloc of bloc
  | Return of expr option

type class_params = ident list
type class_def = ident * class_params option * class_expr option * decl list
type class_main = ident * ident * bloc

type definition =
  | ClassDef of class_def
  | ClassMain of class_main
