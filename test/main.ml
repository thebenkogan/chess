open OUnit2
open Enigma

(** [index_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [index input]. *)
let index_test (name : string) (input : char) (expected_output : int) :
    test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (index input) ~printer:string_of_int

(* You will find it helpful to write functions like [make_index_test]
   for each of the other functions you are testing. They will keep your
   lists of tests below very readable, and will also help you to avoid
   repeating code. You will also find it helpful to create [~printer]
   functions for the data types in use. *)

let r_to_l_test
    (name : string)
    (wiring : string)
    (top_letter : char)
    (input_pos : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (map_r_to_l wiring top_letter input_pos)
    ~printer:string_of_int

let l_to_r_test
    (name : string)
    (wiring : string)
    (top_letter : char)
    (input_pos : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (map_l_to_r wiring top_letter input_pos)
    ~printer:string_of_int

let map_refl_test
    (name : string)
    (wiring : string)
    (input_pos : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (map_refl wiring input_pos)
    ~printer:string_of_int

let map_plug_test
    (name : string)
    (plugs : (char * char) list)
    (c : char)
    (expected_output : char) : test =
  name >:: fun _ -> assert_equal expected_output (map_plug plugs c)

let cipher_test
    (name : string)
    (config : config)
    (c : char)
    (expected_output : char) : test =
  name >:: fun _ -> assert_equal expected_output (cipher_char config c)

let step_test
    (name : string)
    (config : config)
    (expected_output : config) : test =
  name >:: fun _ -> assert_equal expected_output (step config)

let cipher_final_test
    (name : string)
    (config : config)
    (s : string)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (cipher config s)

let reflector_B = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

let reflector_C = "FVPJIAOYEDRZXWGCTKUQSBNMHL"

let rotor_I = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"

let rotor_II = "AJDKSIRUXBLHWTMCQGZNPYFVOE"

let rotor_III = "BDFHJLCPRTXVZNYEIWGAKMUSQO"

let index_tests = []

let map_rl_tests = []

let map_lr_tests = []

let map_refl_tests = []

let map_plug_tests = []

let cipher_char_tests = []

let step_tests = []

let cipher_tests = []

let tests =
  "test suite for A1"
  >::: List.flatten
         [
           index_tests;
           map_rl_tests;
           map_lr_tests;
           map_refl_tests;
           map_plug_tests;
           cipher_char_tests;
           step_tests;
           cipher_tests;
         ]

let _ = run_test_tt_main tests
