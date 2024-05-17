{
  open Parser (* This assumes the parser module is named `Parser` *)
}

let letter = ['a'-'z']
let whitespace = [' ' '\t' '\r' '\n']

rule read = parse
  | whitespace+ { read lexbuf } (* Skip whitespace *)
  | '0'         { ZERO }
  | '1'         { ONE }
  | letter as l { CHAR l }
  | '+'         { PLUS }
  | '*'         { TIMES }
  | '^'         { STAR }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | eof { EOF }
  | _           { failwith "Unexpected character" }
