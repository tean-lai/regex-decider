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
let e2 = Prod [ e1; e1 ]

(* let () = e2 |> Automata.exp_to_nfae |> Automata.print_nfae_info *)
let e3 = Star e1

(* let () = e3 |> Automata.exp_to_nfae |> Automata.print_nfae_info *)

let e4 = Prod [ Star (Prod [ Char 'x'; Char 'y' ]); Char 'x' ]
let e5 = Prod [ Char 'x'; Star (Prod [ Char 'y'; Char 'x' ]) ]
let e6 = Star (Char 'y')

(** [gen_test name expected actual] constructs an OUnit test named
    [name] that asserts the equality of [expected] with [actual]. *)
let gen_test name expected_output actual_output : test =
  name >:: fun _ -> assert_equal expected_output actual_output

let regex_test decide s1 s2 expected =
  let name = if expected then s1 ^ " = " ^ s2 else s1 ^ " <> " ^ s2 in
  let e1 = s1 |> Parse.parse_exp in
  let e2 = s2 |> Parse.parse_exp in
  name >:: fun _ -> assert_equal expected (decide e1 e2)

let parse_test s expected =
  let exp_string = s |> Parse.parse_exp |> string_of_exp in
  let name = s ^ " -> " ^ expected in
  name >:: fun _ -> assert_equal ~printer:(fun x -> x) expected exp_string

let dfa_equiv_tests =
  [ (* gen_test "dfa1 = dfa2" true (Equivalence.decide dfa1 dfa2);
       gen_test "dfa1 <> dfa3" false (Equivalence.decide dfa1 dfa3); *) ]

let parse_tests =
  [
    parse_test "a + b" "Sum [Char a; Char b]";
    parse_test "(x + x) + x" "Sum [Char x; Char x; Char x]";
    parse_test "x + (x + x)" "Sum [Char x; Char x; Char x]";
    parse_test "x^" "Star (Char x)";
    parse_test "x^^" "Star (Star (Char x))";
    parse_test "(x * y)^ * x" "Prod [Star (Prod [Char x; Char y]); Char x]";
    parse_test "x * (y * x)^" "Prod [Char x; Star (Prod [Char y; Char x])]";
    parse_test "(x + y)^" "Star (Sum [Char x; Char y])";
    parse_test "x^ * (y * x^)^"
      "Prod [Star (Char x); Star (Prod [Char y; Star (Char x)])]";
    parse_test "(1 + x) * (1 + x) * (x * x * x)^"
      "Prod [Sum [One; Char x]; Sum [One; Char x]; Star (Prod [Char x; Char x; \
       Char x])]";
    parse_test "" "One";
  ]

let regex_equiv_tests decide =
  [
    regex_test decide "x^" "x^ * x^" true;
    regex_test decide "x^" "x^^" true;
    regex_test decide "(x * y)^ * x" "x * (y * x)^" true;
    regex_test decide "(x + y)^" "x^ * (y * x^)^" true;
    regex_test decide "x^" "(1 + x) * (1 + x) * (x * x * x)^" true;
    regex_test decide "a" "b" false;
    regex_test decide "a + b" "a" false;
    regex_test decide "a + a" "a" true;
    regex_test decide "b + a" "a + b" true;
    regex_test decide "b * a" "a * b" false;
    regex_test decide "a" "a^" false;
    regex_test decide "1 + a * a^" "a^" true;
    regex_test decide "1 + a^ * a" "a^" true;
    regex_test decide "1 * a" "a" true;
    regex_test decide "0 * (a + b)" "0" true;
    regex_test decide "(a + b) + c" "a + (b + c)" true;
    regex_test decide "(a * b) * c" "a * (b * c)" true;
    regex_test decide "(a + b) * c" "a * c + b * c" true;
  ]

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           dfa_equiv_tests;
           regex_equiv_tests Equivalence.decide;
           regex_equiv_tests Equivalence.decide_b;
           parse_tests;
         ]

let _ = run_test_tt_main suite
