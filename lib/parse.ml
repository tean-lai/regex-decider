open Ast

let rec flatten_exp e =
  match e with
  | Sum es ->
      let es' = List.map flatten_exp es in
      Sum (List.flatten (List.map (function Sum es -> es | e -> [ e ]) es'))
  | Prod es ->
      let es' = List.map flatten_exp es in
      Prod (List.flatten (List.map (function Prod es -> es | e -> [ e ]) es'))
  | Star e -> Star (flatten_exp e)
  | Zero | One | Char _ -> e

(* Recursive function to apply flatten_exp until no further changes occur *)
let rec fix_flatten e =
  let flattened = flatten_exp e in
  if flattened = e then e else fix_flatten flattened

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
    try Parser.prog Lexer.read lexbuf |> flatten_exp
    with Parser.Error -> failwith "Parse error"
