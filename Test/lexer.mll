{
  open Parser
  open Lexing
}

let digit = ['0'-'9']
let alpha = ['a'-'z''A'-'Z']

rule token = parse
    ' '         { token lexbuf }
  | '\n'        { EOL }
  | (alpha | '_') (alpha | digit | '_')* as id { IDENT id }
  | digit+ as cnum { CONST (int_of_string cnum) }
  | "**"           { EXP }
  | '%'            { MOD }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { MULT }
  | '/'            { DIV }
  | '='            { AFFECT }
  | eof            { EOF }
