{
  open Error
  open Parser
  open Lexing

  let keyword_table = Hashtbl.create 72
  let _ = 
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
            [("boolean", BOOLEAN); ("class", CLASS); ("else", ELSE); ("extends", EXTENDS);
             ("false", FALSE); ("for", FOR); ("if", IF); ("instanceof", INSTANCEOF);
             ("int", INT); ("native", NATIVE); ("new", NEW); ("null", NULL); ("public", PUBLIC);
             ("return", RETURN); ("static", STATIC); ("this", THIS); ("true", TRUE); ("void", VOID);
             ("main", MAIN)
            ]

  let current_pos lb = 
    (Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb)

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        {  
          pos with pos_bol = lexbuf.lex_curr_pos;
          pos_lnum = pos.pos_lnum + 1
        }
}

  let alpha = ['a'-'z''A'-'Z']
  let chiffre = ['0'-'9']
  let eol = ['\n''\r'] | "\r\n"
  let whitespace = [' ''\t']

  rule token = parse
    | "//"[^'\n']*'\n' { token lexbuf } (* Commentaire sur une ligne *)
    | "/*" { comment (current_pos lexbuf) lexbuf; token lexbuf } (* Commentaire sur plusieurs lignes, imbriqué *)
    | eol { next_line lexbuf; token lexbuf }
    | whitespace { token lexbuf } (* Whitespace, rien a faire *)
    | (alpha | '_')(alpha | '_' | chiffre)* as id
      {
        try
          Hashtbl.find keyword_table id
        with
          Not_found -> IDENT id
      }
    | chiffre+ as cnum { CONST (int_of_string cnum) }
    | '"' { STRING (read_string (current_pos lexbuf) lexbuf) }
    | "++" { INC }
    | "--" { DEC }
    | '+' { PLUS }
    | '*' { MULT }
    | '-' { MINUS }
    | '/' { DIV }
    | '%' { MOD }
    | "==" { EQ }
    | '=' { AFFECT }
    | '!' { NOT }
    | "||" { OR }
    | "&&" { AND }
    | "!=" { NEQ }
    | "<=" { LE }
    | '<' { LT }
    | ">=" { GE }
    | '>' { GT }
    | '(' { LPAR }
    | ')' { RPAR }
    | '.' { DOT }
    | '{' { LEMB }
    | '}' { REMB }
    | ';' { SEMICOLON }
    | ',' { COM }
    | '[' { LBRA }
    | ']' { RBRA }
    | eof { EOF }
    | _ { error (Lexical_error "Character not recognized") (current_pos lexbuf) }

  and comment pos = parse (* Permet de donner plus d'info sur l'erreur, et les commentaires imbriqué *)
    | "/*" { comment pos lexbuf; comment pos lexbuf }
    | "*/" { }
    | eol { next_line lexbuf; comment pos lexbuf }
    | eof { error (Lexical_error "Comment opened but not closed") pos}
    | _ { comment pos lexbuf }

  and read_string pos = parse
    | "\\n" { "\n" ^ (read_string pos lexbuf) }
    | "\\t" { "\t" ^ (read_string pos lexbuf) }
    | "\\\"" { "\"" ^ (read_string pos lexbuf) }
    | "\\\\" { "\\" ^ (read_string pos lexbuf) }
    | '"' { "" }
    | [' '-'~'] as c { (String.make 1 c) ^ (read_string pos lexbuf) }
    | eof { error (Lexical_error "String not terminated") (current_pos lexbuf) }
    | _ as c { error (Lexical_error ("Illegal character '" ^ String.make 1 c ^ "' in string")) (current_pos lexbuf) }
            
{

}
