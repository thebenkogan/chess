open OUnit2
open Chess
open State
open Game
open Helper
open Boards
open Printer

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. TAKEN FROM A2 *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [set_properties bd c] is the game properties with board [bd] and
    color [c] used to test the king-independent movement of a piece.
    [last_move], [enemy_moves], and [king_pos] are all disregarded.
    [king_in_check], [kingside_castle], and [quueenside_castle] are
    false. *)
let set_properties bd c king_pos =
  {
    board = bd;
    color = c;
    last_move = ((-1, -1), (-1, -1));
    enemy_moves = [];
    enemy_find = false;
    king_pos;
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

(** [legal_moves_test name prop ~pin_checker ~move_checker expected_output piece_pos]
    constructs an OUnit test named [name] that asserts that the moves
    from [piece_pos] to [expected_output] has set-like equaltiy with the
    moves from [piece_pos] in
    [legal_moves ~pin_checker ~move_checker prop]. [pin_checker]
    defaults to always returning false, and [move_checker] defaults to
    always returning true. *)
let legal_moves_test
    (name : string)
    (prop : properties)
    ?move_checker:(mc = fun _ _ -> true)
    (expected_output : (int * int) list)
    (piece_pos : int * int) : test =
  let expected_moves = squares_to_moves piece_pos expected_output in
  let output =
    get_piece_moves piece_pos (legal_moves ~move_checker:mc prop)
  in
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:pp_move_list
    expected_moves output

(** [state_test name enemy_moves coords expected_output] constructs an
    OUnit test named [name] that asserts the quality of
    [expected_output] with [init_state board color]. *)
let state_test
    (name : string)
    (board : Game.t)
    (color : Game.color)
    (expected_output : State.t) : test =
  name >:: fun _ ->
  assert_equal expected_output (init_state board color)

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
      (set_properties
         (empty_with_piece (Some (White, Knight)))
         White (-1, -1))
      [ (1, 4); (1, 2); (2, 5); (4, 5); (5, 4); (5, 2); (4, 1); (2, 1) ]
      (3, 3);
    legal_moves_test "Edge of board same and enemy pieces"
      (set_properties knight_board White (-1, -1))
      [ (1, 0); (2, 3); (1, 4) ]
      (0, 2);
  ]

let king_tests =
  [
    legal_moves_test "Middle of empty board"
      (set_properties
         (empty_with_piece (Some (White, King)))
         White (-1, -1))
      [ (4, 4); (4, 3); (3, 2); (3, 4); (2, 3); (2, 2); (2, 4); (4, 2) ]
      (3, 3);
    legal_moves_test "Does not move to attacked squares"
      {
        (set_properties king_board Black (-1, -1)) with
        enemy_moves = king_board_enemy_moves;
      }
      [ (2, 6); (2, 7) ] (3, 7);
  ]

let move_checker_tests =
  [
    legal_moves_test
      "Bishop in line with Knight but King blocked, not pinned"
      (set_properties move_checker_board1 White (4, 0))
      ~move_checker
      [ (1, 0); (0, 3); (1, 4); (3, 4); (4, 3) ]
      (2, 2);
    legal_moves_test "Knight pinned by Bishop"
      (set_properties move_checker_board2 White (4, 0))
      ~move_checker [] (2, 2);
    legal_moves_test "Queen pinned by Rook, can move in line of Rook"
      (set_properties move_checker_board3 Black (3, 7))
      ~move_checker
      [ (3, 0); (3, 1); (3, 2); (3, 3); (3, 4); (3, 5) ]
      (3, 6);
    legal_moves_test "King in check, no blocks or moves"
      (set_properties move_checker_board4 Black (3, 4))
      ~move_checker [] (7, 1);
    legal_moves_test "King in check, pinned piece cannot block"
      (set_properties move_checker_board5 White (4, 0))
      ~move_checker [] (2, 2);
    legal_moves_test "King in check, one move to block check"
      (set_properties move_checker_board6 Black (3, 6))
      ~move_checker [ (3, 5) ] (7, 1);
  ]

let init_properties color =
  {
    board = starting_board;
    color;
    last_move = ((-1, -1), (-1, -1));
    enemy_moves = [];
    enemy_find = false;
    king_pos = (if color = White then (4, 0) else (4, 7));
    king_in_check = false;
    kingside_castle = false;
    queenside_castle = false;
  }

let state_tests =
  [
    state_test "Initialize for white" starting_board White
      {
        game_state = init_properties White;
        moves = starting_board_init_moves;
        turn = true;
        a_rook_moved = false;
        h_rook_moved = false;
        king_moved = false;
      };
    state_test "Initialize for black" starting_board Black
      {
        game_state = init_properties Black;
        moves = [];
        turn = false;
        a_rook_moved = false;
        h_rook_moved = false;
        king_moved = false;
      };
  ]

let bishop_tests =
  [
    legal_moves_test "Middle of empty board"
      (set_properties
         (empty_with_piece (Some (White, Bishop)))
         White (-1, -1))
      bishop_empty_board_coords (3, 3);
    legal_moves_test "Standard board starting position"
      (set_properties starting_board White (-1, -1))
      [] (2, 0);
    legal_moves_test "Bishop check path interference"
      (set_properties bishop_board White (-1, -1))
      [ (1, 1); (2, 0); (1, 3) ]
      (2, 0);
  ]

let rook_tests =
  [
    legal_moves_test "Middle of empty board"
      (set_properties
         (empty_with_piece (Some (White, Rook)))
         White (-1, -1))
      rook_empty_board_coords (3, 3);
    legal_moves_test "Standard board starting position"
      (set_properties starting_board White (-1, -1))
      [] (0, 0);
    legal_moves_test "Rook check path interference"
      (set_properties rook_board White (-1, -1))
      [
        (0, 1);
        (0, 2);
        (0, 3);
        (0, 4);
        (0, 5);
        (0, 6);
        (0, 7);
        (1, 0);
        (1, 2);
      ]
      (0, 0);
  ]

let queen_tests =
  [
    legal_moves_test "Middle of empty board"
      (set_properties
         (empty_with_piece (Some (White, Queen)))
         White (-1, -1))
      queen_empty_board_coords (3, 3);
    legal_moves_test "Standard board starting position"
      (set_properties starting_board White (-1, -1))
      [] (3, 0);
    legal_moves_test "Queen check path interference"
      (set_properties rook_board White (-1, -1))
      queen_path_interference_coords (0, 3);
  ]

let tests =
  "test suite for chess"
  >::: List.flatten
         [
           is_attacked_tests;
           knight_tests;
           king_tests;
           knight_tests;
           rook_tests;
           queen_tests;
           move_checker_tests;
         ]

let _ = run_test_tt_main tests
