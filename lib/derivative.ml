open Ast

let rec ewp (e : exp) : bool =
  match e with
  | Zero -> false
  | One -> true
  | Char _ -> false
  | Sum elist -> List.fold_left (fun acc e -> acc || ewp e) false elist
  | Prod elist -> List.fold_left (fun acc e -> acc && ewp e) true elist
  | Star _ -> true

(* let rec collapse (e : exp) : exp =
     match e with
     | Zero | One | Char _ -> e
     | Sum (e1, e2) -> (
         match (collapse e1, collapse e2) with
         | Zero, Zero -> Zero
         | Zero, e2 -> e2
         | e1, Zero -> e1
         | One, e2 when ewp e2 -> e2
         | e1, One when ewp e1 -> e1
         | e1, e2 -> Sum (e1, e2))
     | Prod (e1, e2) -> (
         match (collapse e1, collapse e2) with
         | Zero, _ -> Zero
         | _, Zero -> Zero
         | One, e2 -> e2
         | e1, One -> e1
         | e1, e2 -> Prod (e1, e2))
     | Star e -> (
         match collapse e with
         | Zero -> Zero
         | One -> One
         | Star e' -> Star e'
         | e -> Star e)

   let rec simplify exp =
     match exp with
     | Sum (e1, e2) -> simplify_sum (simplify e1) (simplify e2)
     | Prod (e1, e2) -> simplify_prod (simplify e1) (simplify e2)
     | Star e -> Star (simplify e)
     | _ -> exp

   and simplify_sum e1 e2 =
     match (e1, e2) with
     | Zero, e | e, Zero -> e
     | Sum (e11, e12), e2 -> Sum (simplify_sum e11 e2, simplify_sum e12 e2)
     | e1, Sum (e21, e22) -> Sum (simplify_sum e1 e21, simplify_sum e1 e22)
     | _ -> Sum (e1, e2)

   and simplify_prod e1 e2 =
     match (e1, e2) with
     | Zero, _ | _, Zero -> Zero
     | One, e | e, One -> e
     | Prod (e11, e12), e2 -> simplify_prod e11 (simplify_prod e12 e2)
     | e1, Prod (e21, e22) -> simplify_prod (simplify_prod e1 e21) e22
     | Sum (e11, e12), e2 -> Sum (simplify_prod e11 e2, simplify_prod e12 e2)
     | e1, Sum (e21, e22) -> Sum (simplify_prod e1 e21, simplify_prod e1 e22)
     | _ -> Prod (e1, e2)

   let normalize exp = simplify exp *)

(* let rec derivative c e : exp =
     (match e with
     | Zero -> Zero
     | One -> Zero
     | Char c' when c = c' -> One
     | Char _ -> Zero
     | Sum elist -> Sum (List.map (fun e -> derivative c e) elist)
     | Prod [] -> failwith "product of nothing"
     | Prod [e1] -> derivative c e1
     | Prod (e1 :: erest) when ewp e1 ->
       Sum (Prod (derivative c e1 :: [Prod erest]) :: [derivative c (Prod erest)])
     | Prod (e1, e2) when ewp e1 ->
         Sum (Prod (derivative c e1, e2), derivative c e2)
     | Prod (e1, e2) -> Prod (derivative c e1, e2)
     | Star e1 -> Prod (derivative c e1, e))
     |> normalize

   let rec lin1 (e : exp) =
     match e with
     | Zero -> Zero
     | One -> Zero
     | Char _ -> e
     | Sum (e1, e2) -> Sum (lin1 e1, lin1 e2)
     | Star e1 -> Prod (lin1 e1, e)
     | Prod (Char _, e1) -> e
     | Prod (Sum (e1, e2), e3) -> Sum (lin1 (Prod (e1, e3)), lin1 (Prod (e2, e3)))
     | Prod (Star e1, e2) -> Sum (Prod (lin1 e1, e), lin1 e2)
     | Prod (Zero, e) -> Zero
     | Prod (One, e) -> lin1 e
     | Prod (Prod (e1, e2), e3) -> lin1 (Prod (e1, Prod (e2, e3)))

   let rec lin2 e =
     match e with
     | Sum (e1, e2) -> Sum (lin2 e1, lin2 e2)
     | Prod (Sum (e1, e2), e3) -> Sum (lin2 (Prod (e1, e3)), lin2 (Prod (e2, e3)))
     | _ -> e *)

let rec det e = failwith "todo1"
let print_exp e = e |> string_of_exp |> print_endline

(* let rec print_derivatives e : unit =
   print_exp e;
   if e = Zero then () else print_derivatives (derivative 'x' e) *)
