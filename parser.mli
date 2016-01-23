type token =
  | INT of (int)
  | OP of (char)

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
