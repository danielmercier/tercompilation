type token =
  | INT of (int)
  | OP of (char)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Lexer
# 10 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* OP *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\004\000"

let yysindex = "\255\255\
\000\255\000\000\000\000\001\255\000\255\001\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\002\000\000\000\004\000"

let yygindex = "\000\000\
\001\000"

let yytablesize = 6
let yytable = "\001\000\
\003\000\003\000\005\000\002\000\000\000\006\000"

let yycheck = "\001\000\
\001\001\000\000\002\001\000\000\255\255\005\000"

let yynames_const = "\
  "

let yynames_block = "\
  INT\000\
  OP\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 11 "parser.mly"
           ( _1 )
# 62 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : char) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 12 "parser.mly"
                  (
      let f = function
        match _2 with
          | '+' -> (+)
          | '-' -> (-)
          | '/' -> (/)
          | '*' -> ( * )
        in
          f _1 _2
      )
# 80 "parser.ml"
               : int))
(* Entry exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : int)
;;
# 22 "parser.mly"
      
# 107 "parser.ml"
