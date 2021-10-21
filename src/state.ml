open Game
open Helper
open Boards

type t = {
  game_state : Game.properties;
  moves : Game.move list;
  enemy_moves : Game.move list;
  turn : bool;
  king_in_check : bool;
  a_rook_moved : bool;
  h_rook_moved : bool;
  king_moved : bool;
}

let init_state (board : Game.t) (color : Game.color) : t =
  let init_properties =
    {
      board = starting_board;
      color;
      last_move = ((-1, -1), (-1, -1));
      king_pos = (if color = White then (4, 0) else (4, 7));
      kingside_castle = false;
      queenside_castle = false;
    }
  in
  let init_moves =
    if color = White then legal_moves init_properties else []
  in
  let init_turn = color = White in
  {
    game_state = init_properties;
    moves = init_moves;
    enemy_moves = [];
    turn = init_turn;
    king_in_check = false;
    a_rook_moved = false;
    h_rook_moved = false;
    king_moved = false;
  }

(** [enemy_properties bd our_color] is the game properties for the
    opponent with the opposite color of [our_color] and board [bd]. The
    [last_move] and [king_pos] are populated with [(-1, -1)], which
    signify that they should be disregarded. [enemy_moves] is the empty
    list to allow free range of the enemy king. [queenside_castle] and
    [kingside_castle] are all false.*)
let enemy_properties bd our_color =
  {
    board = bd;
    color = (if our_color = White then Black else White);
    last_move = ((-1, -1), (-1, -1));
    king_pos = (-1, -1);
    queenside_castle = false;
    kingside_castle = false;
  }

(** [check_a_rook coords color] is true if [coords] is the starting
    position of the rook on the A file depending on [color]. *)
let check_a_rook (oldx, oldy) = function
  | White -> oldx = 0 && oldy = 0
  | Black -> oldx = 0 && oldy = 7

(** [check_h_rook coords color] is true if [coords] is the starting
    position of the rook on the H file depending on [color]. *)
let check_h_rook (oldx, oldy) = function
  | White -> oldx = 7 && oldy = 0
  | Black -> oldx = 7 && oldy = 7

let play_move (st : t) ((oldx, oldy), (newx, newy)) : t =
  let new_last_move = ((oldx, oldy), (newx, newy)) in
  let old_board_arr = board_to_array st.game_state.board in
  let old_piece = old_board_arr.(oldx).(oldy) in
  let new_board = update_board st.game_state.board new_last_move in
  let new_king_pos =
    if old_piece = Some (st.game_state.color, King) then (newx, newy)
    else st.game_state.king_pos
  in
  let new_properties =
    {
      st.game_state with
      board = new_board;
      last_move = new_last_move;
      king_pos = new_king_pos;
    }
  in
  (*If a piece has moved before, it will stay moved. Otherwise, check if
    this is the first move.*)
  let new_a_rook_moved =
    st.a_rook_moved
    || old_piece = Some (st.game_state.color, Rook)
       && check_a_rook (oldx, oldy) st.game_state.color
  in
  let new_h_rook_moved =
    st.h_rook_moved
    || old_piece = Some (st.game_state.color, Rook)
       && check_h_rook (oldx, oldy) st.game_state.color
  in
  let new_king_moved =
    st.king_moved
    || (old_piece = Some (st.game_state.color, King) && oldx = 4)
  in
  {
    game_state = new_properties;
    moves = [];
    enemy_moves = [];
    king_in_check = false;
    a_rook_moved = new_a_rook_moved;
    h_rook_moved = new_h_rook_moved;
    king_moved = new_king_moved;
    turn = false;
  }

let receive_move (st : t) (mv : move) : t =
  let new_board = update_board st.game_state.board mv in
  let enemy_prop = enemy_properties new_board st.game_state.color in
  let new_enemy_moves = legal_moves enemy_prop in
  let new_king_in_check =
    is_attacked new_enemy_moves st.game_state.king_pos
  in
  let new_properties =
    {
      st.game_state with
      board = new_board;
      last_move = mv;
      queenside_castle = false (*TODO*);
      kingside_castle = false (*TODO*);
    }
  in
  let new_moves = legal_moves ~move_checker new_properties in
  {
    st with
    game_state = new_properties;
    moves = new_moves;
    enemy_moves = new_enemy_moves;
    turn = true;
    king_in_check = new_king_in_check;
  }
