open Lexing
open Lexer
open Parser
open Format

let ext = ".java"
let usage = Format.sprintf "usage: %s [options] file%s" Sys.argv.(0) ext

let parse_only = ref false

let spec = ["-parse-only", Arg.Set parse_only, " stops after parsing"]

(* checking for suffix .java in source file's name *)
let file =
    let file = ref None in
    let set_file s =
        if not (Filename.check_suffix s ext) then
            raise (Arg.Bad "Invalid Extension");
        file := Some s
    in
    Arg.parse spec set_file usage;
    match !file with
        | Some f -> f
        | None   -> Arg.usage spec usage ; exit 1

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let ast = Parser.prog Lexer.token lb in
    (*let _ = f lb in*)
    close_in c;
    if !parse_only then exit 0;
  with
    | Error.Error (e,p) ->
      Error.print err_formatter file e p;
      exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
