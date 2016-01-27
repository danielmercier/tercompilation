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
  
type appel = string

type acces =
  | Ident of ident node
  | This
  | Appel_ident of appel node * ident node
  | Acces_ident of acces node * ident node

type expr =
  | Unop of unop * expr node
  | Binop of binop * expr node * expr node
  | Instanceof of expr node * class_expr node
  | Cast of type_ * expr node
  (* instr_expr dans la grammaire *)
  | Affect of acces node * expr node
  | Appel of appel node
  | Post_incr_acces of acces node
  | Pre_incr_acces of acces node
  | Post_decr_acces of acces node
  | Pre_decr_acces of acces node
  | New of class_expr node * (expr node) list
  (********************************)
