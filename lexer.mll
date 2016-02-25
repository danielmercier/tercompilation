{
  open Error
  open Parser
  open Lexing
  open Format

  let keyword_table = Hashtbl.create 72
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
            [("boolean", BOOLEAN);
             ("class", CLASS);
             ("else", ELSE);
             ("extends", EXTENDS);
             ("false", FALSE);
             ("for", FOR);
             ("if", IF);
             ("instanceof", INSTANCEOF);
             ("int", INT);
             ("native", NATIVE);
             ("new", NEW);
             ("null", NULL);
             ("public", PUBLIC);
             ("return", RETURN);
             ("static", STATIC);
             ("this", THIS);
             ("true", TRUE);
             ("void", VOID);
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

  (*strbuf est utile pour read_string, lire une chaine entre guillemet*)
  let strbuf = Buffer.create 80
}

  let alpha = ['a'-'z''A'-'Z']
  let chiffre = ['0'-'9']
  let eol = ['\n''\r'] | "\r\n"
  let whitespace = [' ''\t']
  let ident = (alpha | '_')(alpha | '_' | chiffre)*

  rule token = parse
    (* Commentaire sur une ligne *)
    | "//"[^'\n']*'\n' { token lexbuf } 

    (* Commentaire sur plusieurs lignes *)
    | "/*" { comment (current_pos lexbuf) lexbuf } 

    (* increment du numero de ligne *)
    | eol { next_line lexbuf; token lexbuf } 
    | whitespace { token lexbuf } (* Whitespace, rien a faire *)
    | ident as id
      {
        try
          Hashtbl.find keyword_table id
        with
          Not_found -> IDENT id
      }
    | chiffre+ as cnum { 
        try
          let i = int_of_string cnum in
              if i >= int_of_float (-2. ** 31.) && 
                 i < int_of_float (2. ** 31.)
              then CONST ( Int32.of_int i )
              else 
                  error 
                    (Lexical_error ("integer number too large " ^ cnum))
                    (current_pos lexbuf)
        with Failure _ -> 
          error 
            (Lexical_error ("integer number too large " ^ cnum))
            (current_pos lexbuf)
      }
    | '"' {
        Buffer.reset strbuf;
        read_string (current_pos lexbuf) lexbuf;
        STRING (Buffer.contents strbuf)
    }
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
    | _ {
        error 
          (Lexical_error "Character not recognized") 
          (current_pos lexbuf)
    }

  and comment pos = parse (* Permet de donner plus d'info sur l'erreur *)
    | "*/" { token lexbuf }
    | eol { next_line lexbuf; comment pos lexbuf }
    | eof { error (Lexical_error "Comment opened but not closed") pos}
    | _ { comment pos lexbuf }

  and read_string pos = parse
    | "\\n" { Buffer.add_char strbuf '\n'; read_string pos lexbuf }
    | "\\t" { Buffer.add_char strbuf '\t'; read_string pos lexbuf }
    | "\\\"" { Buffer.add_char strbuf  '"'; read_string pos lexbuf }
    | "\\\\" { Buffer.add_char strbuf '\\'; read_string pos lexbuf }
    | '"' { () }
    | [' '-'~'] as c { Buffer.add_char strbuf c; read_string pos lexbuf }
    | eof {
        error 
          (Lexical_error "String not terminated")
          (current_pos lexbuf) 
    }
    | _ as c {
        error 
          (Lexical_error 
            (sprintf "Illegal character '%c' in string" c))
          (current_pos lexbuf)
    }
