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

let index_tests =
  [
    index_test "index of A is 0" 'A' 0;
    index_test "index of B is 1" 'B' 1;
    index_test "index of Z is 25" 'Z' 25;
    index_test "index of E is 4" 'E' 4;
  ]

let map_rl_tests =
  [
    r_to_l_test "Maps to 0" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 0 0;
    r_to_l_test "Maps to 25" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 25 25;
    r_to_l_test "Maps to 0" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'Z' 0 0;
    r_to_l_test "Maps to 4" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0 4;
    r_to_l_test "Maps to 9" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0 9;
    r_to_l_test "Maps to 25" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 9 25;
    r_to_l_test "Maps to 12" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 9 12;
    r_to_l_test "Maps to 10" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'C' 0 10;
    r_to_l_test "Maps to 9" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'C' 2 9;
    r_to_l_test "Maps to 7" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'D' 24 7;
    r_to_l_test "Maps to 2" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'Z' 23 2;
    r_to_l_test "Maps to 3" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'Y' 24 3;
    r_to_l_test "Maps to 2" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'X' 12 2;
    r_to_l_test "Maps to 17" "BDFHJLCPRTXVZNYEIWGAKMUSQO" 'O' 14 17;
  ]

let map_lr_tests =
  [
    l_to_r_test "Maps to 0" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 0 0;
    l_to_r_test "Maps to 20" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0 20;
    l_to_r_test "Maps to 21" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0 21;
    l_to_r_test "Maps to 14" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'F' 10 14;
    l_to_r_test "Maps to 20" "BDFHJLCPRTXVZNYEIWGAKMUSQO" 'H' 22 20;
    l_to_r_test "Maps to 2" "BDFHJLCPRTXVZNYEIWGAKMUSQO" 'X' 17 2;
  ]

let map_refl_tests =
  [
    map_refl_test "Reflects to 0" "YRUHQSLDPXNGOKMIEBFZCWVJAT" 24 0;
    map_refl_test "Reflects to 7" "YRUHQSLDPXNGOKMIEBFZCWVJAT" 3 7;
    map_refl_test "Reflects to 24" "YRUHQSLDPXNGOKMIEBFZCWVJAT" 0 24;
    map_refl_test "Reflects to 13" "YRUHQSLDPXNGOKMIEBFZCWVJAT" 10 13;
    map_refl_test "Reflects to 19" "YRUHQSLDPXNGOKMIEBFZCWVJAT" 25 19;
  ]

let map_plug_tests =
  [
    map_plug_test "Plugs to Z" [ ('A', 'Z') ] 'A' 'Z';
    map_plug_test "Plugs to Z" [ ('A', 'Z'); ('X', 'Y') ] 'A' 'Z';
    map_plug_test "Plugs to Z" [ ('Y', 'X'); ('Z', 'A') ] 'A' 'Z';
    map_plug_test "Plugs to D"
      [ ('Y', 'X'); ('Z', 'A'); ('C', 'D') ]
      'C' 'D';
    map_plug_test "Plugs to C"
      [ ('Y', 'X'); ('Z', 'A'); ('C', 'D') ]
      'D' 'C';
    map_plug_test "Plugs to A"
      [ ('Y', 'X'); ('Z', 'A'); ('C', 'D') ]
      'Z' 'A';
    map_plug_test "Plugs to Z"
      [ ('Y', 'X'); ('Z', 'A'); ('C', 'D') ]
      'A' 'Z';
  ]

let cipher_char_tests =
  [
    cipher_test "Map to A"
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      }
      'A' 'A';
    cipher_test "Map to B"
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      }
      'B' 'B';
    cipher_test "Map to C"
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      }
      'C' 'C';
    cipher_test "Map to D"
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      }
      'D' 'D';
    cipher_test "Map to E"
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      }
      'E' 'E';
    cipher_test "Map to E"
      { refl = reflector_B; rotors = []; plugboard = [ ('A', 'Z') ] }
      'A' 'D';
  ]

let step_tests =
  [
    step_test "Base example"
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      }
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      };
    step_test "Base example"
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      }
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      };
    step_test "Base example"
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      }
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      };
    step_test "Base example"
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      }
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      };
    step_test "Base example"
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      }
      {
        refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        rotors = [];
        plugboard = [];
      };
  ]

let cipher_tests =
  [
    cipher_final_test "Undo OCaml"
      { refl = reflector_B; rotors = []; plugboard = [ ('A', 'Z') ] }
      "YNGXQ" "Ocaml";
    cipher_final_test "Undo NickRunje"
      { refl = reflector_C; rotors = []; plugboard = [ ('A', 'Z') ] }
      "NickRunje" "DKFLASDFJK";
    cipher_final_test "Undo Cornell"
      {
        refl = reflector_B;
        rotors = [];
        plugboard = [ ('A', 'Z'); ('C', 'N') ];
      }
      "Cornell" "DFAWGJI";
    cipher_final_test "Undo Seattle"
      {
        refl = reflector_B;
        rotors = [];
        plugboard = [ ('A', 'Z'); ('C', 'N') ];
      }
      "Seattle" "GFJGIGL";
    cipher_final_test "Undo America"
      {
        refl = reflector_C;
        rotors = [];
        plugboard = [ ('B', 'Z'); ('C', 'N') ];
      }
      "America" "WRDJOWB";
  ]

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
