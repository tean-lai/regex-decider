open Expression
module IntSet = Set.Make (Int)

module NfaeTrans = Map.Make (
  struct type t = int * char option
  let compare e1 e2 = Int.compare (fst e1) (fst e2)
end
)

type dfa = {
  size : int;
  delta : ((int * char) * int) list;
  start : int;
  final : int list;
}

type nfa = {
  size : int;
  delta : ((int * char) * int) list;
  start : int list;
  final : int list;
}

type nfae = {
  size : int;
  delta : IntSet.t NfaeTrans.t;
  start : IntSet.t;
  final : IntSet.t;
}

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
    (inc_state s, opt_c), IntSet.map inc_state set
  in
  {
    size = a.size;
    delta = NfaeTrans.fold (fun key set acc ->
      let key', set' = inc_transition key set in
      NfaeTrans.add key' set' acc
    ) a.delta NfaeTrans.empty;
    start = IntSet.map inc_state a.start;
    final = IntSet.map inc_state a.final;
  }


(* let inc_all_states_nfae (n : int) (a : nfae) : nfae =
  let help = List.map (fun x -> x + n) in
  {
    size = a.size;
    delta = List.map (fun ((s, c), t) -> ((s + n, c), help t)) a.delta;
    start = help a.start;
    final = help a.final;
  } *)

let rec exp_to_nfae (e : exp) : nfae =
  let module Map = NfaeTrans in
  let merge map1 map2 = 
    Map.fold Map.add map1 map2 in 
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
        delta = Map.add (1, None) (IntSet.of_list [2]) Map.empty;
        start = IntSet.of_list [ 1 ];
        final = IntSet.of_list [ 2 ];
      }
  | Char c ->
      {
        size = 2;
        delta = Map.add (1, Some c) (IntSet.of_list [2]) Map.empty;
        start = IntSet.of_list [ 1 ];
        final = IntSet.of_list [ 2 ];
      }
  | Sum (e1, e2) ->
      let nfa1 = e1 |> exp_to_nfae |> inc_all_states_nfae 1 in
      let nfa2 =
        e2 |> exp_to_nfae |> inc_all_states_nfae (1 + nfa1.size)
      in
      let s, t = (1, 2 + nfa1.size + nfa2.size) in
      {
        size = 2 + nfa1.size + nfa2.size;
        delta = merge nfa1.delta nfa2.delta |>

  Map.add (s, None) (IntSet.union nfa1.start nfa2.start) |> 
  IntSet.fold (fun x acc -> Map.add (x, None) (IntSet.of_list [t]) acc) nfa1.final  |> 
IntSet.fold (fun x acc -> Map.add (x, None) (IntSet.of_list [t]) acc) nfa2.final 
;

        start = IntSet.of_list [ s ];
        final = IntSet.of_list [ t ];
      }
  | Prod (e1, e2) ->
      let nfa1 = e1 |> exp_to_nfae in
      let nfa2 =
        e2 |> exp_to_nfae |> inc_all_states_nfae (nfa1.size - 1)
      in
      {
        size = nfa1.size + nfa2.size - 1;
        delta = merge nfa1.delta nfa2.delta;
        start = IntSet.of_list [ 1 ];
        final = IntSet.of_list [ nfa1.size + nfa2.size - 1 ];
      }
  | Star e ->
      let nfa = e |> exp_to_nfae |> inc_all_states_nfae 1 in
      let s, t = (1, nfa.size + 2) in
      {
        size = nfa.size + 2;
        delta =
          nfa.delta |> 
          Map.add (s, None) (IntSet.add t nfa.start) |> 
          IntSet.fold (fun x acc -> Map.add (x, None) nfa.final acc) nfa.start;
        start = IntSet.of_list [ s ];
        final = IntSet.of_list [ t ];
      }

(* let nfa_accept (nfa : nfa) (s : string) : bool = let rec help nfa s
   st = match s with | None -> st = nfa.final | Some (c, s') -> let st'
   = List.assoc (st, c) nfa.delta in help nfa s' st' in help nfa (s |>
   String.to_seq |> Seq.uncons) 1 *)

let rec ( ** ) x n =
  if n = 0 then 1
  else if x mod 2 = 0 then
    let half_pow = x ** (n / 2) in
    half_pow * half_pow
  else x * (x ** (n - 1))

let rec bit_length n = if n = 0 then 0 else 1 + bit_length (n lsr 1)

let list_to_state lst =
  let lst = List.sort_uniq Int.compare lst in
  List.fold_left (fun acc x -> acc + (1 lsl (x - 1))) 0 lst

let state_to_list st =
  let rec help i acc =
    if i = 0 then acc
    else if (st lsr (i - 1)) land 1 > 0 then help (i - 1) (i :: acc)
    else help (i - 1) acc
  in
  help (bit_length st) []

let nfae_to_dfa (nfae : nfae) : dfa =
  let t = 2 ** nfae.size in
  { size = t; delta = []; start = 1; final = [ t ] }

let exp_to_dfa (e : exp) : dfa = failwith "todo"
