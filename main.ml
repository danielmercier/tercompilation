open Lexing
open Lexer
open Parser
open Format

let file = "test"
let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let ast = Parser.prog Lexer.token lb in
    (*let _ = f lb in*)
    close_in c;
    exit 0
  with
    | Error.Error (e,p) ->
      Error.print err_formatter file e p;
      exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
