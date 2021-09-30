open OUnit2

(** [index_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [index input]. *)
(* SAMPLE TEST HELPER FUNCTION: let index_test (name : string) (input :
   char) (expected_output : int) : test = name >:: fun _ -> assert_equal
   expected_output (index input) ~printer:string_of_int *)

let sample_tests = []

let tests = "test suite for A1" >::: List.flatten [ sample_tests ]

let _ = run_test_tt_main tests
