open Regex.Automata
open Regex.Ast
open Regex.Equivalence
open Regex.Parse
open Regex.Derivative

let help s =
  let exp_string = s |> parse_exp |> string_of_exp in
  print_endline (s ^ " -> " ^ exp_string)

let help2 s1 s2 =
  print_string (s1 ^ " = " ^ s2 ^ ": ");
  let equiv1 = decide_w_automaton s1 s2 in
  let equiv2 = decide_w_brzowski s1 s2 in
  print_endline (string_of_bool equiv1 ^ " " ^ string_of_bool equiv2)

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
  print_endline "\nNow we do problem 1 of homework 1 (a-e),";
  print_endline
    "The first boolean is the result from automaton conversion the second is \
     directly using derivatives";
  help2 "x^" "x^ * x^";
  help2 "x^" "x^^";
  help2 "(x * y)^ * x" "x * (y * x)^";
  help2 "(x + y)^" "x^ * (y * x^)^";
  help2 "x^" "(1 + x) * (1 + x) * (x * x * x)^"

let () =
  print_endline "\nNow we do more examples";
  help2 "a" "b";
  help2 "a + b" "a";
  help2 "a + a" "a";
  help2 "b + a" "a + b";
  help2 "b * a" "a * b";
  help2 "a" "a^";
  help2 "1 + a * a^" "a^";
  help2 "1 + a^ * a" "a^";
  help2 "1 * a" "a";
  help2 "0 * (a + b)" "0";
  help2 "(a + b) + c" "a + (b + c)";
  help2 "(a * b) * c" "a * (b * c)";
  help2 "(a + b) * c" "a * c + b * c"
