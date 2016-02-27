open Lexing
open Lexer
open Parser
open Format
open Error
open Ast

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
    (* Vérification du nom de la classe *)
    let (_, (classname, _, _)) = ast in
    let f2 = List.hd (List.rev (Str.split (Str.regexp "/") file)) in
    let name = String.sub f2 0 (String.length f2 - String.length ext) in
    let () =
        if name <> classname.value then
            error (Syntax_error
                    (Some (Format.sprintf "the public class \"%s\" containing the main function should have the same name as the file \"%s\"" classname.value name)))
                  (classname.pos)
    in
    close_in c;
    if !parse_only then exit 0;
  with
    | Error.Error (e,p) ->
      Error.print err_formatter file e p;
      exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
