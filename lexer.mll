{
  open Error

  type token =
    | IDENT of string
    | INT of int
    | STRING of string
    | PLUS | MINUS | MULT | DIV
    | BOOLEAN | CLASS | ELSE
    | EXTENDS | FALSE | FOR
    | IF | INSTANCEOF | TINT
    | NATIVE | NEW | NULL
    | PUBLIC | RETURN | STATIC
    | THIS | TRUE | VOID | EOF

  let keyword_table = Hashtbl.create 72
  let _ = 
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
            [("boolean", BOOLEAN); ("class", CLASS); ("else", ELSE); ("extends", EXTENDS);
             ("false", FALSE); ("for", FOR); ("if", IF); ("instanceof", INSTANCEOF);
             ("int", TINT); ("native", NATIVE); ("new", NEW); ("null", NULL); ("public", PUBLIC);
             ("return", RETURN); ("static", STATIC); ("this", THIS); ("true", TRUE); ("void", VOID)]

  let current_pos lb = 
    (Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb)
}

  let alpha = ['a'-'z''A'-'Z']
  let chiffre = ['0'-'9']

  rule token = parse
    | "//"[^'\n']*'\n' { token lexbuf } (* Commentaire sur une ligne *)
    | "/*" { comment (current_pos lexbuf) lexbuf } (* Commentaire sur plusieurs lignes *)
    | [' ''\t''\n'] { token lexbuf } (* Whitespace, rien a faire *)
    | (alpha | '_')(alpha | '_' | chiffre)* as id
      {
        try
          Hashtbl.find keyword_table id
        with
          Not_found -> IDENT id
      }
    | chiffre+ as cnum
      {
        let num = int_of_string cnum in
        INT num
      }
    | '"'([' '-'~']* as str)'"' { STRING str }
    | '+' { PLUS }
    | '*' { MULT }
    | '-' { MINUS }
    | '/' { DIV }
    | eof { EOF }
    | _ { error (Lexical_error "Character not recognized") (current_pos lexbuf) }

  and comment pos = parse (* Permet de donner plus d'info sur l'erreur *)
    | "*/" { token lexbuf }
    | eof { error (Lexical_error "Comment opened but not closed") pos}
    | _ { comment pos lexbuf }

{

}
