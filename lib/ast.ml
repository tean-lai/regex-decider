type exp =
  | Zero
  | One
  | Char of char
  | Sum of exp list
  | Prod of exp list
  | Star of exp

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
