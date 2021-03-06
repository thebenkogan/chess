open OUnit2
open Chess
open State
open Game
open Helper
open Boards

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

(** [pp_move mv] pretty-prints the move [mv] as it is read as a tuple. *)
let pp_move mv =
  let x1 = string_of_int (fst (fst mv)) in
  let y1 = string_of_int (snd (fst mv)) in
  let x2 = string_of_int (fst (snd mv)) in
  let y2 = string_of_int (snd (snd mv)) in
  "((" ^ x1 ^ ", " ^ y1 ^ "), (" ^ x2 ^ ", " ^ y2 ^ "))"

(** [pp_move_list lst] pretty-prints list [lst], using [pp_move] to
    pretty-print each move. TAKEN FROM A2.*)
let pp_move_list lst =
  let pp_elt = pp_move in
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [set_properties bd c king_pos] is the game properties with board
    [bd] and color [c] used to test the king-independent movement of a
    piece. [last_move], [enemy_moves], and [king_pos] are all
    disregarded. [king_in_check], [kingside_castle], and
    [queenside_castle] are false. *)
let set_properties
    ?last_move:(lm = ((-1, -1), (-1, -1)))
    (bd : Game.t)
    (c : Game.color)
    (king_pos : int * int)
    (king_in_check : bool) =
  {
    board = bd;
    color = c;
    last_move = lm;
    king_pos;
    king_in_check;
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

(** [legal_moves_test name prop ~move_checker expected_output piece_pos]
    constructs an OUnit test named [name] that asserts that the moves
    from [piece_pos] to [expected_output] has set-like equaltiy with the
    moves from [piece_pos] in [legal_moves ~move_checker prop].
    [move_checker] defaults to always returning true. *)
let legal_moves_test
    (name : string)
    (prop : properties)
    ?pin_checker:(pc = fun _ _ _ -> None)
    ?move_checker:(mc = fun _ _ -> true)
    (expected_output : (int * int) list)
    (piece_pos : int * int) : test =
  let expected_moves = squares_to_moves piece_pos expected_output in
  let output =
    get_piece_moves piece_pos
      (legal_moves ~pin_checker:pc ~move_checker:mc prop)
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

(** [update_board_test name bd move expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [update_board bd move]. *)
let update_board_test
    (name : string)
    (bd : t)
    (move : move)
    (expected_output : t) : test =
  name >:: fun _ -> assert_equal expected_output (update_board bd move)

let set_castle_properties bd =
  {
    board = bd;
    color = White;
    last_move = ((-1, -1), (-1, -1));
    king_pos = (4, 0);
    king_in_check = false;
    kingside_castle = true;
    queenside_castle = true;
  }

let set_castle_state gs a_rook h_rook king_moved =
  {
    game_state = gs;
    moves = [];
    enemy_moves = [];
    a_rook_moved = a_rook;
    h_rook_moved = h_rook;
    king_moved;
  }

(** [castle_test name state mv expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with the
    kingside and queenside castling rights of [receive_move state mv]. *)
let castle_test
    (name : string)
    (state : State.t)
    (mv : move)
    (expected_output : bool * bool) : test =
  let new_state = receive_move state mv in
  name >:: fun _ ->
  assert_equal expected_output
    ( new_state.game_state.kingside_castle,
      new_state.game_state.queenside_castle )

let castle_tests =
  [
    castle_test "Can castle"
      (set_castle_state
         (set_castle_properties castle1)
         false false false)
      ((1, 1), (1, 2))
      (true, true);
    castle_test "king moved"
      (set_castle_state
         (set_castle_properties castle1)
         false false true)
      ((1, 1), (1, 2))
      (false, false);
    castle_test "a rook moved"
      (set_castle_state
         (set_castle_properties castle1)
         true false false)
      ((1, 1), (1, 2))
      (true, false);
    castle_test "h rook moved"
      (set_castle_state
         (set_castle_properties castle1)
         false true false)
      ((1, 1), (1, 2))
      (false, true);
    castle_test "blocked by piece"
      (set_castle_state
         (set_castle_properties castle2)
         false false false)
      ((1, 1), (1, 2))
      (false, false);
    castle_test "square in check"
      (set_castle_state
         (set_castle_properties castle3)
         false false false)
      ((1, 1), (1, 2))
      (false, false);
    castle_test "King in check"
      (set_castle_state
         (set_castle_properties castle4)
         false false false)
      ((1, 1), (1, 2))
      (false, false);
    castle_test "Rook captured"
      (set_castle_state
         (set_castle_properties castle5)
         true false false)
      ((6, 2), (7, 1))
      (false, false);
  ]

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
         White (-1, -1) false)
      [ (1, 4); (1, 2); (2, 5); (4, 5); (5, 4); (5, 2); (4, 1); (2, 1) ]
      (3, 3);
    legal_moves_test "Edge of board same and enemy pieces"
      (set_properties knight_board White (-1, -1) false)
      [ (1, 0); (2, 3); (1, 4) ]
      (0, 2);
  ]

let king_tests =
  [
    legal_moves_test "Middle of empty board"
      (set_properties
         (empty_with_piece (Some (White, King)))
         White (-1, -1) false)
      [ (4, 4); (4, 3); (3, 2); (3, 4); (2, 3); (2, 2); (2, 4); (4, 2) ]
      (3, 3);
    legal_moves_test "Does not move to attacked squares"
      (set_properties king_board1 Black (3, 7) false)
      ~pin_checker ~move_checker [ (2, 6); (2, 7) ] (3, 7);
    legal_moves_test "Does not move to attacked squares in line"
      (set_properties king_board2 Black (4, 5) true)
      ~pin_checker ~move_checker
      [ (3, 5); (3, 4); (3, 6); (5, 5); (5, 6); (5, 4) ]
      (4, 5);
    legal_moves_test "Can move in front of pawn, not corners"
      (set_properties king_board3 Black (4, 5) false)
      ~pin_checker ~move_checker
      [ (4, 4); (4, 6); (3, 6); (5, 6); (3, 5); (5, 5) ]
      (4, 5);
    legal_moves_test "Cannot take protected piece"
      (set_properties king_board4 Black (4, 5) false)
      ~pin_checker ~move_checker
      [ (4, 6); (5, 6); (5, 5) ]
      (4, 5);
  ]

let move_checker_tests =
  [
    legal_moves_test
      "Bishop in line with Knight but King blocked, not pinned"
      (set_properties move_checker_board1 White (4, 0) false)
      ~pin_checker ~move_checker
      [ (1, 0); (0, 3); (1, 4); (3, 4); (4, 3) ]
      (2, 2);
    legal_moves_test "Knight pinned by Bishop"
      (set_properties move_checker_board2 White (4, 0) false)
      ~pin_checker ~move_checker [] (2, 2);
    legal_moves_test "Queen pinned by Rook, can move in line of Rook"
      (set_properties move_checker_board3 Black (3, 7) false)
      ~pin_checker ~move_checker
      [ (3, 0); (3, 1); (3, 2); (3, 3); (3, 4); (3, 5) ]
      (3, 6);
    legal_moves_test "King in check, no blocks or moves"
      (set_properties move_checker_board4 Black (3, 4) true)
      ~pin_checker ~move_checker [] (7, 1);
    legal_moves_test "King in check, pinned piece cannot block"
      (set_properties move_checker_board5 White (4, 0) true)
      ~pin_checker ~move_checker [] (2, 2);
    legal_moves_test "King in check, one move to block check"
      (set_properties move_checker_board6 Black (3, 6) true)
      ~pin_checker ~move_checker [ (3, 5) ] (7, 1);
    legal_moves_test "En passant move blocked"
      (set_properties move_checker_board7 White (1, 4) true
         ~last_move:((3, 6), (3, 4)))
      ~pin_checker ~move_checker [ (2, 5) ] (2, 4);
  ]

let init_properties color =
  {
    board = starting_board;
    color;
    last_move = ((-1, -1), (-1, -1));
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
        enemy_moves = [];
        a_rook_moved = false;
        h_rook_moved = false;
        king_moved = false;
      };
    state_test "Initialize for black" starting_board Black
      {
        game_state = init_properties Black;
        moves = [];
        enemy_moves = [];
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
         White (-1, -1) false)
      bishop_empty_board_coords (3, 3);
    legal_moves_test "Standard board starting position"
      (set_properties starting_board White (-1, -1) false)
      [] (2, 0);
    legal_moves_test "Bishop check path interference"
      (set_properties bishop_board White (-1, -1) false)
      [ (1, 1); (2, 0); (1, 3) ]
      (2, 0);
  ]

let rook_tests =
  [
    legal_moves_test "Middle of empty board rook"
      (set_properties
         (empty_with_piece (Some (White, Rook)))
         White (-1, -1) false)
      rook_empty_board_coords (3, 3);
    legal_moves_test "Standard board starting position"
      (set_properties starting_board White (-1, -1) false)
      [] (0, 0);
    legal_moves_test "Rook check path interference"
      (set_properties rook_board White (-1, -1) false)
      rook_path_interference_coords (0, 0);
  ]

let queen_tests =
  [
    legal_moves_test "Middle of empty board queen"
      (set_properties
         (empty_with_piece (Some (White, Queen)))
         White (-1, -1) false)
      queen_empty_board_coords (3, 3);
    legal_moves_test "Standard board starting position"
      (set_properties starting_board White (-1, -1) false)
      [] (3, 0);
    legal_moves_test "Queen check path interference"
      (set_properties queen_board White (-1, -1) false)
      queen_path_interference_coords (3, 0);
  ]

let pawn_tests =
  [
    legal_moves_test "Middle of empty board pawn"
      (set_properties
         (empty_with_piece (Some (White, Pawn)))
         White (-1, -1) false)
      [ (3, 4) ] (3, 3);
    legal_moves_test "Forward from standard board"
      (set_properties starting_board White (-1, -1) false)
      [ (5, 2); (5, 3) ] (5, 1);
    legal_moves_test "Forward from standard board other position"
      (set_properties starting_board White (-1, -1) false)
      [ (2, 2); (2, 3) ] (2, 1);
    legal_moves_test "Basic capture"
      (set_properties starting_board_pawn1 White (-1, -1) false)
      [ (2, 5); (1, 5) ] (2, 4);
    legal_moves_test "En passant move"
      (set_properties starting_board_pawn2 White (-1, -1) false
         ~last_move:((1, 6), (1, 4)))
      [ (2, 5); (1, 5) ] (2, 4);
  ]

let update_board_tests =
  [
    update_board_test "Simple pawn forward one from starting board"
      starting_board
      ((1, 1), (1, 2))
      starting_board_update1;
    update_board_test "Simple pawn forward two from starting board"
      starting_board
      ((1, 1), (1, 3))
      starting_board_update2;
  ]

let tests =
  "test suite for chess"
  >::: List.flatten
         [
           update_board_tests;
           pawn_tests;
           is_attacked_tests;
           knight_tests;
           king_tests;
           knight_tests;
           rook_tests;
           queen_tests;
           move_checker_tests;
           state_tests;
           castle_tests;
         ]

let _ = run_test_tt_main tests
