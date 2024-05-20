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

(* let rec compress e =
     match e with
     | Zero | One | Char _ -> e
     | Sum [] -> Zero
     | Sum [ e' ] -> e'
     | Sum es ->
         Sum
           (List.map compress es
           |> List.filter (fun x -> x = Zero)
           |> List.sort_uniq compare)
     | Prod [] -> One
     | Prod [ e' ] -> e'
     | Prod es -> Prod (es |> List.map compress |> List.filter (fun x -> x = One))
     | Star e' -> compress e'

   let fix_up e = e |> flatten |> compress *)

let rec normalize e =
  match e with
  | Zero -> Zero
  | One -> One
  | Char c -> Char c
  | Star e' -> Star (normalize e')
  | Sum es -> normalize_sum (List.map normalize es)
  | Prod es -> normalize_prod (List.map normalize es)

and normalize_sum es =
  let flat_es = flatten_sum es in
  let sorted_es = List.sort_uniq Stdlib.compare flat_es in
  match sorted_es with [] -> Zero | [ e ] -> e | _ -> Sum sorted_es

and normalize_prod es =
  let flat_es = flatten_prod es in
  match flat_es with [] -> One | [ e ] -> e | _ -> Prod flat_es

and flatten_sum es =
  List.fold_left
    (fun acc e -> match e with Sum nested -> acc @ nested | _ -> acc @ [ e ])
    [] es

and flatten_prod es =
  List.fold_left
    (fun acc e -> match e with Prod nested -> acc @ nested | _ -> acc @ [ e ])
    [] es

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
