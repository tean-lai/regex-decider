open Ast
module IntSet = Set.Make (Int)

let alph = "abcdefghijklmnopqsrtuvwxyz" |> String.to_seq |> List.of_seq

module EpsTrans = Map.Make (struct
  type t = int * char option

  let compare (s1, c1_opt) (s2, c2_opt) =
    if s1 <> s2 then Int.compare s1 s2
    else
      match (c1_opt, c2_opt) with
      | None, None -> 0
      | None, _ -> 1
      | _, None -> -1
      | Some c1, Some c2 -> Char.compare c1 c2
end)

module Trans = Map.Make (struct
  type t = int * char

  let compare (s1, c1) (s2, c2) =
    if s1 <> s2 then Int.compare s1 s2 else Char.compare c1 c2
end)

type dfa = { size : int; delta : int Trans.t; start : int; final : IntSet.t }

type nfa = {
  size : int;
  delta : IntSet.t Trans.t;
  start : IntSet.t;
  final : IntSet.t;
}

type nfae = {
  size : int;
  delta : IntSet.t EpsTrans.t;
  start : IntSet.t;
  final : IntSet.t;
}

let print_intset intset =
  print_string "Intset: ";
  IntSet.iter
    (fun x ->
      print_int x;
      print_string " ")
    intset;
  print_endline ""

let print_nfae_info (nfae : nfae) =
  print_string "Number of states: ";
  print_int nfae.size;
  print_endline "";
  print_endline "Transitions:";
  EpsTrans.iter
    (fun (s, c) v ->
      print_string "\td(";
      print_int s;
      print_string ", ";
      print_char (c |> Option.value ~default:'0');
      print_string ") = ";
      IntSet.iter
        (fun x ->
          print_string " ";
          print_int x)
        v;
      print_endline "")
    nfae.delta;
  print_string "Start:";
  IntSet.iter
    (fun x ->
      print_string " ";
      print_int x)
    nfae.start;
  print_endline "";
  print_string "Final:";
  IntSet.iter
    (fun x ->
      print_string " ";
      print_int x)
    nfae.final;
  print_endline "\n"

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** Increases the every state number in a nfa by [n]. So if an automata
    had states 0, 1, 2. Increasing it by 3 would turn them into 3, 4, 5. *)
let inc_all_states_nfae (n : int) (a : nfae) : nfae =
  let inc_state s = s + n in
  let inc_transition (s, opt_c) set =
    ((inc_state s, opt_c), IntSet.map inc_state set)
  in
  {
    size = a.size;
    delta =
      EpsTrans.fold
        (fun key set acc ->
          let key', set' = inc_transition key set in
          EpsTrans.add key' set' acc)
        a.delta EpsTrans.empty;
    start = IntSet.map inc_state a.start;
    final = IntSet.map inc_state a.final;
  }

let inc_all_states_dfa n (dfa : dfa) : dfa =
  let inc_state s = s + n in
  let inc_transition (s, c) t = ((inc_state s, c), inc_state t) in
  {
    size = dfa.size;
    delta =
      Trans.fold
        (fun k v acc ->
          let k', v' = inc_transition k v in
          Trans.add k' v' acc)
        dfa.delta Trans.empty;
    start = inc_state dfa.start;
    final = IntSet.map inc_state dfa.final;
  }

let rec exp_to_nfae (e : exp) : nfae =
  let module Map = EpsTrans in
  let merge map1 map2 = Map.fold Map.add map1 map2 in
  match e with
  | Zero ->
      {
        size = 2;
        delta = Map.empty;
        start = IntSet.of_list [ 1 ];
        final = IntSet.of_list [ 2 ];
      }
  | One ->
      {
        size = 2;
        delta = Map.add (1, None) (IntSet.of_list [ 2 ]) Map.empty;
        start = IntSet.of_list [ 1 ];
        final = IntSet.of_list [ 2 ];
      }
  | Char c ->
      {
        size = 2;
        delta = Map.add (1, Some c) (IntSet.of_list [ 2 ]) Map.empty;
        start = IntSet.of_list [ 1 ];
        final = IntSet.of_list [ 2 ];
      }
  | Sum [] -> failwith "shouldn't happen"
  | Sum [ e1 ] -> exp_to_nfae e1
  | Sum (e1 :: erest) ->
      let nfa1 = e1 |> exp_to_nfae |> inc_all_states_nfae 1 in
      let nfa2 =
        Sum erest |> exp_to_nfae |> inc_all_states_nfae (1 + nfa1.size)
      in
      let s, t = (1, 2 + nfa1.size + nfa2.size) in
      {
        size = 2 + nfa1.size + nfa2.size;
        delta =
          merge nfa1.delta nfa2.delta
          |> Map.add (s, None) (IntSet.union nfa1.start nfa2.start)
          |> IntSet.fold
               (fun x acc -> Map.add (x, None) (IntSet.of_list [ t ]) acc)
               nfa1.final
          |> IntSet.fold
               (fun x acc -> Map.add (x, None) (IntSet.of_list [ t ]) acc)
               nfa2.final;
        start = IntSet.of_list [ s ];
        final = IntSet.of_list [ t ];
      }
  | Prod [] -> failwith "shouldn't happen"
  | Prod [ e1 ] -> exp_to_nfae e1
  | Prod (e1 :: erest) ->
      let nfa1 = e1 |> exp_to_nfae in
      let nfa2 =
        Prod erest |> exp_to_nfae |> inc_all_states_nfae (nfa1.size - 1)
      in
      {
        size = nfa1.size + nfa2.size - 1;
        delta = merge nfa1.delta nfa2.delta;
        start = IntSet.of_list [ 1 ];
        final = IntSet.of_list [ nfa1.size + nfa2.size - 1 ];
      }
  | Star e ->
      let subnfae = e |> exp_to_nfae |> inc_all_states_nfae 1 in
      (* print_nfae_info subnfae; *)
      let s, t = (1, subnfae.size + 2) in
      {
        size = subnfae.size + 2;
        delta =
          subnfae.delta
          |> Map.add (s, None) (IntSet.add t subnfae.start)
          |> IntSet.fold
               (fun x acc -> Map.add (x, None) (IntSet.add t subnfae.start) acc)
               subnfae.final;
        start = IntSet.of_list [ s ];
        final = IntSet.of_list [ t ];
      }

let rec ( ** ) x n =
  if n = 0 then 1
  else if n mod 2 = 0 then
    let half_pow = x ** (n / 2) in
    half_pow * half_pow
  else x * (x ** (n - 1))

let rec bit_length n = if n = 0 then 0 else 1 + bit_length (n lsr 1)

let list_to_state lst =
  let lst = List.sort_uniq Int.compare lst in
  List.fold_left (fun acc x -> acc + (1 lsl (x - 1))) 1 lst

let set_to_state set =
  (* if not (IntSet.is_empty set) then (
     print_intset set;
     print_string "\t->";
     print_int (set |> IntSet.elements |> list_to_state);
     print_endline ""); *)
  set |> IntSet.elements |> list_to_state

let state_to_list st =
  let rec help i acc =
    if i = 0 then acc
    else if (st lsr (i - 1)) land 1 > 0 then help (i - 1) (i :: acc)
    else help (i - 1) acc
  in
  help (bit_length st) []

let epsilon_closure nfae states =
  let rec helper closure queue visited =
    match queue with
    | [] -> closure
    | state :: rest ->
        let new_states =
          EpsTrans.find_opt (state, None) nfae.delta
          |> Option.value ~default:IntSet.empty
        in
        let new_states = IntSet.(diff (diff new_states closure) visited) in

        let closure' = IntSet.union new_states closure in
        let visited' = IntSet.add state visited in
        let queue' = IntSet.elements new_states @ rest in
        helper closure' queue' visited'
  in
  helper states (IntSet.elements states) IntSet.empty

let nfae_move nfae states symbol =
  IntSet.fold
    (fun state acc ->
      let target_states =
        EpsTrans.find_opt (state, Some symbol) nfae.delta
        |> Option.value ~default:IntSet.empty
      in
      IntSet.union acc (epsilon_closure nfae target_states))
    states IntSet.empty

let nfae_to_dfa nfae : dfa =
  let initial_closure = epsilon_closure nfae nfae.start in
  let rec process queue (visited : Int.t list) dfa_states dfa_finals
      dfa_transitions : dfa =
    match queue with
    | [] ->
        {
          size = 2 ** nfae.size;
          delta = dfa_transitions;
          start = set_to_state initial_closure;
          final = dfa_finals;
        }
    | state_set :: queue_rest ->
        let state_set_id = set_to_state state_set in
        (* print_intset state_set;
           print_string "-> Int ";
           print_int state_set_id;
           print_endline ""; *)
        if List.mem state_set_id visited then
          process queue_rest visited dfa_states dfa_finals dfa_transitions
        else
          (* print_string "Processing ";
             print_int state_set_id;
             print_endline ""; *)
          let visited' = state_set_id :: visited in
          let state_transitions, new_queue =
            List.fold_left
              (fun (trans, q) symbol ->
                let target_set = nfae_move nfae state_set symbol in
                let target_set_id = set_to_state target_set in
                if not (List.mem target_set_id visited || List.mem target_set q)
                then
                  ( Trans.add (state_set_id, symbol) target_set_id trans,
                    target_set :: q )
                else (Trans.add (state_set_id, symbol) target_set_id trans, q))
              (dfa_transitions, queue_rest)
              alph
          in
          let finals' =
            if IntSet.inter state_set nfae.final != IntSet.empty then
              IntSet.add state_set_id dfa_finals
            else dfa_finals
          in
          process new_queue visited' dfa_states finals' state_transitions
  in
  process [ initial_closure ] [] [] IntSet.empty Trans.empty

let exp_to_dfa (e : exp) : dfa =
  let nfae = exp_to_nfae e in
  nfae_to_dfa nfae

let is_final (dfa : dfa) s : bool = IntSet.mem s dfa.final
let dfa_read (dfa : dfa) c s : int option = Trans.find_opt (s, c) dfa.delta

let create_dfa size transitions start final : dfa =
  (* let size =
       IntSet.of_list final |> IntSet.add start
       |> List.fold_right
            (fun ((s, c), t) acc -> acc |> IntSet.add s |> IntSet.add t)
            transitions
       |> IntSet.cardinal
     in *)
  {
    size;
    delta =
      List.fold_left
        (fun acc ((s, c), t) -> Trans.add (s, c) t acc)
        Trans.empty transitions;
    start;
    final = IntSet.of_list final;
  }

let dfa_size (dfa : dfa) = dfa.size
let dfa_start (dfa : dfa) = dfa.start

let print_dfa_info (dfa : dfa) =
  print_string "Number of states: ";
  print_int dfa.size;
  print_endline "";
  print_endline "Transitions:";
  Trans.iter
    (fun (s, c) v ->
      print_string "\td(";
      print_int s;
      print_string ", ";
      print_char c;
      print_string ") = ";
      print_int v;
      print_endline "")
    dfa.delta;
  print_string "Start: ";
  print_int dfa.start;
  print_endline "";
  print_string "Final:";
  IntSet.iter
    (fun x ->
      print_string " ";
      print_int x)
    dfa.final;
  print_endline "\n"
