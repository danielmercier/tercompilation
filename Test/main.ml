let () =
  let lb = Lexing.from_channel stdin in
  Parser.file Lexer.token lb

