type position = Lexing.position * Lexing.position

type ident = string
type 'a node = { value: 'a; pos: position }

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
  | Post_incr of expr node
  | Pre_incr of expr node
  | Post_decr of expr node
  | Pre_decr of expr node
  | New of class_expr node * (expr node) list
  (********************************)

  (* acces dans la grammaire *)
  | Ident of ident node
  | This
  | Dot_acces of expr node * ident node
  (***************************)

  (* appel dans la grammaire *)
  | Appel of expr node * (expr node) list
  (***************************)


