open OUnit2
open Chess
open State
open Game
open Helper
open Boards
open Printer

(** [set_properties bd c] is the game properties with board [bd] and
    color [c] used to test the king-independent movement of a piece.
    [last_move], [enemy_moves], and [king_pos] are all disregarded.
    [king_in_check], [kingside_castle], and [quueenside_castle] are
    false. *)
let set_properties bd c =
  {
    board = bd;
    color = c;
    last_move = ((-1, -1), (-1, -1));
    enemy_moves = [];
    king_pos = (-1, -1);
    king_in_check = false;
    kingside_castle = false;
    queenside_castle = false;
  }

(** [is_attacked_test name enemy_moves coords expected_output]
    constructs an OUnit test named [name] that asserts the quality of
    [expected_output] with [is_attacked enemy_moves coords]. *)
let is_attacked_test
    (name : string)
    (enemy_moves : move list)
    (coords : int * int)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (is_attacked enemy_moves coords)

(** [is_subset lst1 lst2] is true if [lst1] is a subset of [lst2].
    [lst1] is a subset if all of its elements are in [lst2] regardless
    of multiplicity. If [lst1] is empty, this is true. *)
let rec is_subset lst1 lst2 =
  match lst1 with
  | [] -> true
  | h :: t -> List.mem h lst2 && is_subset t lst2

(** [legal_moves_test name prop ~pin_checker ~move_checker expected_output piece_pos]
    constructs an OUnit test named [name] that asserts that the moves
    from [piece_pos] to [expected_output] is a subset of
    [legal_moves ~pin_checker ~move_checker prop]. [pin_checker]
    defaults to always returning false, and [move_checker] defaults to
    always returning true. *)
let legal_moves_test
    (name : string)
    (prop : properties)
    ?pin_checker:(pc = fun _ _ -> false)
    ?move_checker:(mc = fun _ _ -> true)
    (expected_output : (int * int) list)
    (piece_pos : int * int) : test =
  let expected_moves = squares_to_moves piece_pos expected_output in
  name >:: fun _ ->
  assert_equal ~cmp:is_subset ~printer:pp_move_list expected_moves
    (legal_moves ~pin_checker:pc ~move_checker:mc prop)

let is_attacked_tests =
  [
    is_attacked_test "Not attacked" [ ((7, 6), (3, 4)) ] (2, 3) false;
    is_attacked_test "Attacked once" [ ((7, 6), (2, 3)) ] (2, 3) true;
    is_attacked_test "Attacked multiple times"
      [ ((7, 6), (2, 3)); ((4, 5), (2, 3)) ]
      (2, 3) true;
  ]

let knight_tests =
  [
    legal_moves_test "Middle of empty board"
      (set_properties (empty_with_piece (Some (White, Knight))) White)
      [ (1, 4); (1, 2); (2, 5); (4, 5); (5, 4); (5, 2); (4, 1); (2, 1) ]
      (3, 3);
    legal_moves_test "Edge of board same and enemy pieces"
      (set_properties knight_board White)
      [ (1, 0); (2, 3); (1, 4) ]
      (0, 2);
  ]

let tests =
  "test suite for chess"
  >::: List.flatten [ is_attacked_tests; knight_tests ]

let _ = run_test_tt_main tests
