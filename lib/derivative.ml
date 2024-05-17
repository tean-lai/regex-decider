open Ast

let rec ewp (e : exp) : bool =
  match e with
  | Zero -> false
  | One -> true
  | Char _ -> false
  | Sum (e1, e2) -> ewp e1 || ewp e2
  | Prod (e1, e2) -> ewp e1 && ewp e2
  | Star _ -> true

let rec collapse (e : exp) : exp =
  match e with
  | Zero | One | Char _ -> e
  | Sum (Zero, e1) | Sum (e1, Zero) -> collapse e1
  | Sum (e1, e2) -> collapse (Sum (collapse e1, collapse e2))
  | Prod (Zero, _) | Prod (_, Zero) -> Zero
  | Prod (One, e) | Prod (e, One) -> e
  | Prod (e1, e2) -> collapse (Prod (collapse e1, collapse e2))
  | _ -> failwith "todo"

let rec derivative c e : exp =
  match e with
  | Zero -> Zero
  | One -> Zero
  | Char c' -> if c = c' then One else Zero
  | Sum (e1, e2) -> Sum (derivative c e1, derivative c e2)
  | Prod (e1, e2) ->
      if ewp e1 then Sum (Prod (derivative c e1, e2), derivative c e2)
      else Prod (derivative c e1, e2)
  | Star e1 -> Prod (derivative c e1, e)

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
  | _ -> e

let rec det e = failwith "todo"
