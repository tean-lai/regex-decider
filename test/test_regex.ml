open OUnit2
open Regex
open Regex.Ast

let dfa1 =
  Automata.create_dfa 2
    [ ((1, 'a'), 2); ((1, 'b'), 1); ((2, 'a'), 2); ((2, 'b'), 1) ]
    1 [ 2 ]

let dfa2 =
  Automata.create_dfa 3
    [
      ((1, 'a'), 2);
      ((1, 'b'), 3);
      ((2, 'a'), 2);
      ((2, 'b'), 3);
      ((3, 'a'), 2);
      ((3, 'b'), 1);
    ]
    1 [ 2 ]

let dfa3 =
  Automata.create_dfa 2
    [ ((1, 'a'), 2); ((1, 'b'), 1); ((2, 'a'), 2); ((2, 'b'), 1) ]
    1 [ 1 ]

(* let e0 = Char 'x'
   let e0nfae = Automata.exp_to_nfae e0
   let e0nfae = Automata.inc_all_states_nfae 10 e0nfae *)
(* let () = Automata.print_nfae_info e0nfae *)
let e1 = Star (Char 'x')
let e1nfae = Automata.exp_to_nfae e1
let e1dfa = Automata.nfae_to_dfa e1nfae

(* let () = Automata.print_nfae_info e1nfae *)
(* let () = e1nfae |> Automata.inc_all_states_nfae 10 |> Automata.print_nfae_info *)
(* let () = print_endline "\n\n\n" *)
let e2 = Prod (e1, e1)

(* let () = e2 |> Automata.exp_to_nfae |> Automata.print_nfae_info *)
let e3 = Star e1

(* let () = e3 |> Automata.exp_to_nfae |> Automata.print_nfae_info *)
let e4 = Prod (Star (Prod (Char 'x', Char 'y')), Char 'x')
let e5 = Prod (Char 'x', Star (Prod (Char 'y', Char 'x')))
let e6 = Star (Char 'y')

(** [gen_test name expected actual] constructs an OUnit test named
    [name] that asserts the equality of [expected] with [actual]. *)
let gen_test name expected_output actual_output : test =
  name >:: fun _ -> assert_equal expected_output actual_output

let dfa_equiv_tests =
  [
    gen_test "dfa1 = dfa2" true (Equivalence.decide dfa1 dfa2);
    gen_test "dfa1 <> dfa3" false (Equivalence.decide dfa1 dfa3);
  ]

let regex_equiv_tests =
  [
    gen_test "x* = x**" true
      (Equivalence.decide (Automata.exp_to_dfa e1) (Automata.exp_to_dfa e3));
    gen_test "x** = x*" true
      (Equivalence.decide (Automata.exp_to_dfa e3) (Automata.exp_to_dfa e1));
    gen_test "x* <> y*" false
      (Equivalence.decide (Automata.exp_to_dfa e1) (Automata.exp_to_dfa e6));
    gen_test "y* <> x*" false
      (Equivalence.decide (Automata.exp_to_dfa e6) (Automata.exp_to_dfa e1));
    gen_test "x* = x*x*" true
      (Equivalence.decide (Automata.exp_to_dfa e1) (Automata.exp_to_dfa e2));
    gen_test "x(yx)* = (xy)*x" true
      (Equivalence.decide (Automata.exp_to_dfa e5) (Automata.exp_to_dfa e4));
    gen_test "(xy)*x = (xy)*x" true
      (Equivalence.decide (Automata.exp_to_dfa e4) (Automata.exp_to_dfa e4));
    gen_test "x + x = x" true
      (Equivalence.decide
         (Automata.exp_to_dfa (Sum (Char 'x', Char 'x')))
         (Automata.exp_to_dfa (Char 'x')));
  ]

let suite =
  "test suite for A2" >::: List.flatten [ dfa_equiv_tests; regex_equiv_tests ]

let _ = run_test_tt_main suite
