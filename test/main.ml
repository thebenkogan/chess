open OUnit2
open Chess
open State
open Game
open Boards

(* SAMPLE TEST HELPER FUNCTION: let index_test (name : string) (input :
   char) (expected_output : int) : test = name >:: fun _ -> assert_equal
   expected_output (index input) ~printer:string_of_int *)

(** [start_room_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [start_room input]. *)
let is_attacked_test
    (name : string)
    (enemy_moves : move list)
    (coords : int * int)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (is_attacked enemy_moves coords)

(*It is impossible for the opponent to not have any legal moves and the
  game continues, therefore there is no need to test it.*)
let is_attacked_tests =
  [
    is_attacked_test "Not attacked" [ ((7, 6), (3, 4)) ] (2, 3) false;
    is_attacked_test "Attacked once" [ ((7, 6), (2, 3)) ] (2, 3) true;
    is_attacked_test "Attacked multiple times"
      [ ((7, 6), (2, 3)); ((4, 5), (2, 3)) ]
      (2, 3) true;
  ]

let tests =
  "test suite for chess" >::: List.flatten [ is_attacked_tests ]

let _ = run_test_tt_main tests
