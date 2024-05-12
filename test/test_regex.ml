open OUnit2
open Regex

(** [gen_test name expected actual] constructs an OUnit test named
    [name] that asserts the equality of [expected] with [actual]. *)
let gen_test name expected_output actual_output : test =
  name >:: fun _ -> assert_equal expected_output actual_output

let suite =
  "test suite for A2" >::: List.flatten [ [ gen_test "hi" 0 4 ] ]

let _ = run_test_tt_main suite
