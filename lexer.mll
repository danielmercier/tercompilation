
(* HEADER *)
{
    open Lexing
    open Parser

    exception SyntaxError of string
    exception error_in_lexer of string 

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with pos_bol  = lexbuf.lex_curr_pos;
                       pos_lnum = pos.pos_lnum + 1
            }

    type token = 
    | BOOLEAN_KW | INT_KW
    | CLASS | EXTENDS | PUBLIC | STATIC
    | IF | ELSE
    | TRUE | FALSE
    | FOR
    | INSTANCEOF
    | NATIVE
    | NEW
    | NULL
    | VOID | RETURN | MAIN
    | THIS
    | IDENT of string
    | INT * int
    | FLOAT * float
    | CHAR of char
    | STRING of string
    | OP of op
    | COMMA | SEMI | DOT
    | L_BRACKET | R_BRACKET
    | L_PAREN | R_PAREN
    | L_BRACE | R_BRACE
    | EQ | NEQ | LT | LE | GT | GE | PLUS | MINUQ | MUL | DIV | MOD | NOT | AND | OR
    | INCR | DECR


    let keyword_table =
        let tbl = Hashtbl.create 10 in
        List.iter (fun (key, data) -> Hashtbl.add tbl key data) [
            ("boolean",BOOLEAN_KW);
            ("class",CLASS);
            ("else",ELSE);
            ("extends",EXTENDS);
            ("false",FALSE);
            ("for",FOR);
            ("if",IF);
            ("instanceof",INSTANCEOF);
            ("int",INT_KW);
            ("main",MAIN);
            ("native",NATIVE);
            ("new",NEW);
            ("null",NULL);
            ("public",PUBLIC);
            ("return",RETURN);
            ("static",STATIC);
            ("this",THIS);
            ("true",TRUE);
            ("void",VOID)
        ];
        tbl

    let symbol_table =
        let tbl = Hashtbl.create 10 in
        List.iter (fun (key, data) -> Hashtbl.add tbl key data) [
        ];
        tbl
}

(* DEFINITIONS *)
let digit = ['0' - '9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | '_' | digit)*
let int = '-'? digit+
let float = '-'? digit* ('.' digit*)? (['e' 'E'] ['-''+']? digit+)?

let symbols = (";" | "[" | "]" | "(" | ")" | "{" | "}")

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let oneLineComment = "//" [^ '\n' eof ]*


(* RULES *)
rule read_common = parse
    | whitespace     { read lexbuf }
    | newline        { next_line lexbuf; read lexbuf }
    | int            { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | float          { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | ident          { try
                           Hashtbl.find keyword_table Lexing.lexeme
                       with Not_found -> IDENT Lexing.lexeme
                     }
    | '"'            { read_string (Buffer.create 20) lexbuf }
    | symbols        { try
                           Hashtbl.find symbol_table Lexing.lexeme
                       with Not_found -> raise (error_in_lexer("Error : symbol not found in hashtbl"))
    | oneLineComment { read lexbuf }
    | "/*"           { read_comment lexbuf }
    | ","            { COMMA }
    | ";"            { SEMI }
    | "."            { DOT }
    | "["            { L_BRACKET }
    | "]"            { R_BRACKET }
    | "("            { L_PAREN }
    | ")"            { R_PAREN }
    | "{"            { L_BRACE }
    | "}"            { R_BRACE }
    | "="            { AFFECT }
    | "=="           { EQ }
    | "!="           { NEQ }
    | '<'            { LT }
    | "<="           { LE }
    | '>'            { GT }
    | ">="           { GE }
    | '+'            { PLUS }
    | '-'            { MINUS }
    | '*'            { MUL }
    | '/'            { DIV }
    | '%'            { MOD }
    | '!'            { NOT }
    | "&&"           { AND }
    | "||"           { OR }
    | "true"         { TRUE }
    | "false"        { FALSE }
    | "++"           { INCR }
    | "--"           { DECR }
    | "null"         { NULL }
    | _              { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof            { EOF }
    
and read_string buf = parse
    | '"'       { STRING (Buf.contents buf) }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | '\\' '"'  { Buffer.add_char buf '"' ; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | [^ '\\' '"']+ { 
        {   Buffer.add_string buf (Lexing.lexeme lexbuf);
            read_string buf lexbuf
        }
    | _         { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("String is not terminated")) }

and read_comment = parse
    | "*/" { read_common lexbuf }
    | _    { read_comment lexbuf }





