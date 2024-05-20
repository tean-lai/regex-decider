open Automata
open Ast
open Derivative
module Uf = Unionfind
(* include Queue *)

let alph = "abcdefghijklmnopqsrtuvwxyz" |> String.to_seq |> List.of_seq

let print (a, b) =
  print_int a;
  print_string " ";
  print_int b;
  print_endline ""

let rec hk_loop uf queue dfa1 dfa2 =
  if Queue.is_empty queue then true
  else
    let p, q = Queue.pop queue in
    if is_final p dfa1 <> is_final q dfa2 then false
    else (
      List.map
        (fun a ->
          match (dfa_read p a dfa1, dfa_read q a dfa2) with
          | None, _ | _, None -> ()
          | Some p', Some q' ->
              if Uf.find uf p' <> Uf.find uf q' then (
                Uf.union uf p' q';
                Queue.push (p', q') queue))
        alph
      |> ignore;
      hk_loop uf queue dfa1 dfa2)

let decide e1 e2 =
  let dfa1, dfa2 = (exp_to_dfa e1, exp_to_dfa e2) in
  let dfa2 = inc_all_states_dfa (dfa_size dfa1) dfa2 in
  let dfa1start, dfa2start = (dfa_start dfa1, dfa_start dfa2) in
  let uf = Uf.create () in
  let q = Queue.create () in
  Queue.add (dfa1start, dfa2start) q;
  hk_loop uf q dfa1 dfa2

let decide_b e1 e2 = equiv (ExpExpSet.of_list [ (e1, e2) ]) ExpExpSet.empty

(* let rec decide2_helper uf queue =
     if Queue.is_empty queue then true
     else
       let e1, e2 = Queue.pop queue in
       print_endline ((e1 |> string_of_exp) ^ "\n" ^ (e2 |> string_of_exp) ^ "\n\n");
       for i = 0 to 1000000000 do
         ()
       done;
       if ewp e1 <> ewp e2 then false
       else (
         List.map
           (fun c ->
             let e1', e2' = (derivative c e1, derivative c e2) in
             if Uf.find uf e1' <> Uf.find uf e2' then (
               Uf.union uf e1' e2';
               Queue.push (e1', e2') queue))
           alph
         |> ignore;
         decide2_helper uf queue)

   (* let decide2 e1 e2 =
     let uf = Uf.create () in
     let q = Queue.create () in
     Queue.add (e1, e2) q;
     decide2_helper uf q *) *)

(* let rec decide2_helper visited e1 e2 =
     if ExpSet.mem (e1, e2) visited then true
     else
       let visited = ExpSet.add (e1, e2) visited in
       if ewp e1 <> ewp e2 then false
       else
         List.for_all
           (fun c ->
             let d1, d2 = (derivative c e1, derivative c e2) in
             decide2_helper visited d1 d2)
           alph

   let decide2 e1 e2 = decide2_helper ExpSet.empty e1 e2 *)

(* let rec hk_loop q =
   match Queue.is_empty q with
   | true ->
       let x', y' = Queue.front q in
       if equiv x' y' then
         skip
     else
       failwith "todo"
   | false -> true *)

(** We use Hopcroft-Karp to do bisimulation. 
    (https://hal.science/hal-00639716v2/file/hkc.pdf) *)
(* let decide (dfa1 : dfa) (dfa2 : dfa) : bool =
   let q = Queue.empty |> Queue.enqueue (dfa1.start, dfa2.start) in
   hk_loop q *)
