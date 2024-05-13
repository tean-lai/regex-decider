open Automata
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
    (* print_endline "running"; *)
    let p, q = Queue.pop queue in
    if is_final p dfa1 <> is_final q dfa2 then
      (* print_int p;
         print_string " ";
         print_int q;
         print_endline ""; *)
      false
    else (
      List.map
        (fun a ->
          match (dfa_read p a dfa1, dfa_read q a dfa2) with
          | None, _ -> ()
          | _, None -> ()
          | Some p', Some q' ->
              if Uf.find uf p' <> Uf.find uf q' then (
                Uf.union uf p' q';
                Queue.push (p', q') queue))
          (* let p', q' = (Uf.find uf p', Uf.find uf q') in
             if p' <> q' then (
               Uf.union uf p' q';
               Queue.push (p', q') queue)) *)
        alph
      |> ignore;
      hk_loop uf queue dfa1 dfa2)

let decide (dfa1 : dfa) (dfa2 : dfa) : bool =
  let dfa2 = inc_all_states_dfa (dfa_size dfa1) dfa2 in
  let dfa1s, dfa2s = (dfa_start dfa1, dfa_start dfa2) in

  (* print_dfa_info dfa1;
     print_dfa_info dfa2; *)
  let uf = Uf.create () in
  (* print (dfa1s, dfa2s); *)
  Uf.make_set uf dfa1s;
  Uf.make_set uf dfa2s;
  let q = Queue.create () in
  Queue.add (dfa1s, dfa2s) q;
  hk_loop uf q dfa1 dfa2

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
