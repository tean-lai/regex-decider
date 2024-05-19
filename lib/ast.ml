type exp =
  | Zero
  | One
  | Char of char
  | Sum of exp list
  | Prod of exp list
  | Star of exp

let rec flatten e =
  match e with
  | Zero | One | Char _ -> e
  | Sum es ->
      let es' = List.map flatten es in
      Sum (List.flatten (List.map (function Sum es -> es | e -> [ e ]) es'))
  | Prod es ->
      let es' = List.map flatten es in
      Prod (List.flatten (List.map (function Prod es -> es | e -> [ e ]) es'))
  | Star e -> Star (flatten e)

let rec string_of_exp e =
  match e with
  | Zero -> "Zero"
  | One -> "One"
  | Char c -> Printf.sprintf "Char %c" c
  | Sum es ->
      Printf.sprintf "Sum [%s]" (String.concat "; " (List.map string_of_exp es))
  | Prod es ->
      Printf.sprintf "Prod [%s]"
        (String.concat "; " (List.map string_of_exp es))
  | Star e -> Printf.sprintf "Star (%s)" (string_of_exp e)
