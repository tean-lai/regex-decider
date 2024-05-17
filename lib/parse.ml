let parse (lexbuf : Lexing.lexbuf) : Ast.exp = Parser.prog Lexer.read lexbuf

let parse_exp s =
  if s = "" then Ast.One
  else
    let lexbuf = Lexing.from_string s in
    try Parser.prog Lexer.read lexbuf
    with Parser.Error -> failwith "Parse error"
