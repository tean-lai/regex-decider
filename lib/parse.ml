open Ast

let rec sort_unique e =
  match e with
  | Zero | One | Char _ | Prod _ | Star _ -> e
  | Sum es ->
      let es' = List.map sort_unique es in
      Sum (List.sort_uniq compare es')

let parse (lexbuf : Lexing.lexbuf) : exp = Parser.prog Lexer.read lexbuf

let parse_exp s =
  if s = "" then One
  else
    let lexbuf = Lexing.from_string s in
    try Parser.prog Lexer.read lexbuf |> flatten
    with Parser.Error -> failwith "Parse error"
