type exp =
  | Zero
  | One
  | Char of char
  | Sum of exp * exp
  | Prod of exp * exp
  | Star of exp

let rec string_of_exp e =
  match e with
  | Zero -> "Zero"
  | One -> "One"
  | Char c -> Printf.sprintf "Char(%c)" c
  | Sum (e1, e2) ->
      Printf.sprintf "Sum(%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | Prod (e1, e2) ->
      Printf.sprintf "Prod(%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | Star e -> Printf.sprintf "Star(%s)" (string_of_exp e)
