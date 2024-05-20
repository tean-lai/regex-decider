open Ast

let rec ewp (e : exp) : bool =
  match e with
  | Zero -> false
  | One -> true
  | Char _ -> false
  | Sum es -> List.exists ewp es
  | Prod es -> List.for_all ewp es
  | Star _ -> true

let rec derivative c e =
  match e with
  | Zero -> Zero
  | One -> Zero
  | Char c' when c = c' -> One
  | Char _ -> Zero
  | Sum es -> Sum (List.map (derivative c) es)
  | Prod [] -> Zero
  | Prod (e :: es) ->
      let d = derivative c e in
      let rest = Prod es in
      if ewp e then Sum [ Prod [ d; rest ]; derivative c (Prod es) ]
      else Prod [ d; rest ]
  | Star e -> Prod [ derivative c e; Star e ]

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

(* module ExpSet = Set.Make (struct
     type t = exp

     let compare = compare
   end)

   let concat e (p, e') =
     match e' with Prod es -> (p, Prod (e :: es)) | _ -> (p, Prod [ e; e' ])

   let concat' e1 e2 =
     match (e1, e2) with
     | Prod e1s, Prod e2s -> Prod (e1s @ e2s)
     | Prod e1s, _ -> Prod (e1s @ [ e2 ])
     | _, Prod e2s -> Prod (e1 :: e2s)
     | _, _ -> Prod [ e1; e2 ]

   module CharExpSet = Set.Make (struct
     type t = char * exp

     let compare = compare
   end)

   let build_sum es = match es with [] -> Zero | [ e ] -> e | es' -> Sum es'
   let build_prod es = match es with [] -> One | [ e ] -> e | _ -> Prod es

   let rec f e =
     match e with
     | Zero | One -> []
     | Char c -> [ (c, One) ]
     | Sum es -> List.fold_left (fun acc x -> x @ acc) [] (List.map f es)
     | Star e' -> List.map (concat e) (f e')
     | Prod [] -> []
     | Prod (Zero :: erest) -> []
     | Prod (One :: erest) -> f (build_prod erest)
     | Prod (Prod e1 :: erest) -> f (Prod (e1 @ erest))
     | Prod (Char c :: erest) -> [ (c, Prod erest) ]
     | Prod (Sum es :: erest) ->
         let concatenations = List.map (concat' (build_prod erest)) es in
         List.map f concatenations |> List.fold_left (fun acc x -> x @ acc) []
     | Prod (Star e' :: erest) -> List.map (concat e) (f e') @ f (Prod erest)

   let rec f e =
     let open CharExpSet in
     let big_union = List.fold_left (fun acc x -> union acc x) empty in

     match e with
     | Zero | One -> empty
     | Char c -> of_list [ (c, One) ]
     | Sum es ->
         let es = List.sort_uniq Stdlib.compare es in
         big_union (List.map f (List.map flatten es))
     | Star e' -> map (concat e) (f (flatten e'))
     | Prod (Char c :: erest) -> of_list [ (c, build_prod erest) ]
     | Prod (Sum es :: erest) ->
         let concatenations =
           List.map (concat' (build_prod erest)) (List.sort_uniq Stdlib.compare es)
         in
         concatenations |> List.map flatten |> List.map f |> big_union
     | Prod (Star e' :: erest) ->
         union (map (concat e) (f (flatten e'))) (f (build_prod erest))
     | Prod _ -> failwith "shoulnd't happen"

   let rec g' p (ls : (char * exp) list) : exp list =
     List.filter (fun (p', _) -> p = p') ls |> List.map snd

   let g e =
     let lr : (char * exp) list = f e |> CharExpSet.to_list in
     List.map (fun (p, _) -> (p, build_sum (g' p lr))) lr |> CharExpSet.of_list

   let rec der (p : char) (lr : (char * exp) list) =
     match lr with
     | [] -> Zero
     | (p', e) :: _ when p' = p -> e
     | _ :: tl -> der p tl

   let derivatives (set1 : CharExpSet.t) set2 =
     let lst1, lst2 = CharExpSet.(to_list set1, to_list set2) in
     List.map (fun (p, _) -> (p, der p lst1, der p lst2)) (lst1 @ lst2)

   module ExpExpSet = Set.Make (struct
     type t = exp * exp

     let compare = compare
   end)

   let rec equiv s h =
     let open ExpExpSet in
     match to_list s with
     | [] -> true
     | (e1, e2) :: tl when ewp e1 <> ewp e2 -> false
     | (e1, e2) :: tl ->
         let h' = add (e1, e2) h in
         let s' =
           derivatives (g e1) (g e2)
           |> List.map (fun (_, der1, der2) -> (der1, der2))
           |> List.filter (fun (der1, der2) -> not (mem (der1, der2) h'))
           |> of_list
         in
         equiv (union (of_list tl) s') h' *)

let print_exp e = e |> string_of_exp |> print_endline

(* let rec print_derivatives e : unit =
   print_exp e;
   if e = Zero then () else print_derivatives (derivative 'x' e) *)
