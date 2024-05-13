open Regex.Automata
open Regex.Ast
open Regex.Equivalence
open Regex.Parse

let help s =
  let exp_string = s |> parse_exp |> string_of_exp in
  print_endline (s ^ " -> " ^ exp_string)

let help2 s1 s2 =
  print_string (s1 ^ " = " ^ s2 ^ ": ");
  let exp1, exp2 = (parse_exp s1, parse_exp s2) in
  let dfa1, dfa2 = (exp_to_dfa exp1, exp_to_dfa exp2) in
  let equiv = decide dfa1 dfa2 in
  print_endline (string_of_bool equiv)

let () =
  print_endline "Check out the regex parsing!\n";
  help "a + b";
  help "(x + x) + x";
  help "x + (x + x)";
  help "x^";
  help "x^^";
  help "(x * y)^ * x";
  help "x * (y * x)^";
  help "(x + y)^";
  help "x^ * (y * x^)^";
  help "(1 + x) * (1 + x) * (x * x * x)^"

let () =
  print_endline "\n\nNow we do problem 1 of homework 1 (a-e)";
  help2 "x^" "x^ * x^";
  help2 "x^" "x^^";
  help2 "(x * y)^ * x" "x * (y * x)^";
  help2 "(x + y)^" "x^ * (y * x^)^";
  help2 "x^" "(1 + x) * (1 + x) * (x * x * x)^"

let () =
  print_endline "\n\nNow we do counterexamples";
  help2 "a" "b";
  help2 "a + b" "a"
