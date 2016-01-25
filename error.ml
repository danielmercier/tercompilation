open Lexing
open Ast
open Format

type error = 
  | Lexical_error of string
  | Syntax_error
  (*| Interpretation_error
  | Unknown_identifier of string*)

exception Error of error * Ast.position

let report_loc fmt file (b,e) =
  if b = dummy_pos || e = dummy_pos then
  fprintf fmt "File \"%s\"\nerror: " file
  else
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  fprintf fmt "File \"%s\", line %d, characters %d-%d\nerror: " file l fc lc

let to_string e =
  match e with
    | Lexical_error s -> sprintf "lexical error: %s" s
    | Syntax_error -> sprintf "syntax error"
    (*| Interpretation_error -> sprintf "interpretation error"
    | Unknown_identifier id -> sprintf "unknown identifier %s" id*)

let print fmt f e p =
  report_loc fmt f p;
  fprintf fmt "%s\n@." (to_string e)

let error e p = raise (Error (e, p))
