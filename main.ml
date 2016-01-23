open Lexing
open Lexer
open Format

let token_to_string = function
  | IDENT s -> "Ident : " ^ s
  | INT i -> "Int : " ^ string_of_int i
  | STRING s -> "Chaine : " ^ s
  | OP o -> "Opération : " ^ Char.escaped o
  | BOOLEAN -> "boolean" | CLASS -> "class" | ELSE -> "else"
  | EXTENDS -> "extends" | FALSE -> "false" | FOR -> "for"
  | IF -> "if" | INSTANCEOF -> "instanceof" | TINT -> "tint"
  | NATIVE -> "native" | NEW -> "new" | NULL -> "null"
  | PUBLIC -> "public" | RETURN -> "return" | STATIC -> "static"
  | THIS -> "this" | TRUE -> "true" | VOID -> "void" | EOF -> "END"

let rec f lb =
  let t = Lexer.token lb in
  let s = token_to_string t in
  print_string s;
  print_newline ();
  if s <> "END"
  then f lb
  else ()

let file = "test"
let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let _ = f lb in
    close_in c;
    exit 0
  with
    | Error.Error (e,p) ->
      Error.print err_formatter file e p;
      exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
