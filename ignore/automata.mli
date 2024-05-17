open Expression

type nfae
type nfa
type dfa

val inc_all_states_dfa : int -> dfa -> dfa
val inc_all_states_nfae : int -> nfae -> nfae

val exp_to_nfae : exp -> nfae
(** [exp_to_nfae e] converts [e] into a nfa with epsilon transitions via 
    Thompson's construction *)

val exp_to_dfa : exp -> dfa
(** [exp_to_dfa e] converts [e] into a dfa *)

val nfae_to_dfa : nfae -> dfa

val is_final : int -> dfa -> bool
(** [is_final s dfa] returns if s is one of the final states of [dfa] *)

val dfa_read : int -> char -> dfa -> int option
(** [dfa_read s c dfa] delta(s, c) as an option *)

val create_dfa : int -> ((int * char) * int) list -> int -> int list -> dfa
val dfa_size : dfa -> int
val dfa_start : dfa -> int
val print_dfa_info : dfa -> unit
val print_nfae_info : nfae -> unit
