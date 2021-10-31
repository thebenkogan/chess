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

(** [castling_rights st] is the kingside and queenside castling rights
    for the player in [st]. Kingside castling is legal if the first
    element of the output is true, and queenside castling is legal if
    the second element of the output is true. A player can castle in a
    certain direciton if:

    - The king and the corresponding rook have not moved.
    - All squares between the king and rook are empty.
    - The king is not in check.
    - All the squares in between the king's current location and target
      destination (inclusive) are not attacked.
    - Note: The corresponding rook may be attacked, this only requires
      the king's path is clear.

    NOTE: This implementation can be agnostic to color. You can make
    separate functions for kingside and queenside detection, but you do
    not need to do it by color. Everything can be done relative to the
    current king_pos, found in st.game_state.king_pos. Use the
    is_attacked function in game.ml and the other state variables to
    check each condition. enemy_moves will help you check if squares are
    attacked, king_in_check will tell you if our king is currently in
    check, and a/h_rook_moved and king_moved will give you all the
    context for the pieces.*)
let castling_rights (st : t) : bool * bool =
  if st.king_moved || st.king_in_check then (false, false)
  else (kingside_castle_rights st, queenside_castle_rights st)

let kingside_castle_rights (st: t): bool =
  let square1 = (fst st.game_state.king_pos + 1, snd st.game_state.king_pos) in
  let square2 = (fst st.game_state.king_pos + 2, snd st.game_state.king_pos) in
  let board_arr = board_to_array st.game_state.board in
  let piece1 = board_arr.(fst square1).(snd square1) in
  let piece2 = board_arr(fst square2).(snd square2) in
  if piece1 = Some(st.game_state.color, Knight) || (**etc. can soldier instead?, need to check black knight and bishop otherwise*)
     piece1 = Some(st.game_state.color, Bishop) ||
     piece1 = Some(st.game_state.color, Rook) ||
     piece1 = Some(st.game_state.color, Queen) ||
     piece2 = Some(st.game_state.color, Knight) ||
     piece2 = Some(st.game_state.color, Bishop) ||
     piece2 = Some(st.game_state.color, Rook) ||
     piece2 = Some(st.game_state.color, Queen) ||
     st.game_state.h_rook_moved ||
     is_attacked st.game_state.enemy_moves square1 ||
     is_attacked st.game_state.enemy_moves square2
     then false
  else true


let queenside_castle_rights (st: t): bool =
  let square1 = (fst st.game_state.king_pos - 1, snd st.game_state.king_pos) in
  let square2 = (fst st.game_state.king_pos - 2, snd st.game_state.king_pos) in
  let board_arr = board_to_array st.game_state.board in
  let piece1 = board_arr.(fst square1).(snd square1) in
  let piece2 = board_arr(fst square2).(snd square2) in
  if piece1 = Some(st.game_state.color, Knight) || (**etc. can soldier instead?, need to check black knight and bishop otherwise*)
      piece1 = Some(st.game_state.color, Bishop) ||
      piece1 = Some(st.game_state.color, Rook) ||
      piece1 = Some(st.game_state.color, Queen) ||
      piece2 = Some(st.game_state.color, Knight) ||
      piece2 = Some(st.game_state.color, Bishop) ||
      piece2 = Some(st.game_state.color, Rook) ||
      piece2 = Some(st.game_state.color, Queen) ||
      st.game_state.h_rook_moved ||
      is_attacked st.game_state.enemy_moves square1 ||
      is_attacked st.game_state.enemy_moves square2
      then false
  else true

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
  let can_castle = castling_rights st in
  let new_properties =
    {
      st.game_state with
      board = new_board;
      last_move = mv;
      kingside_castle = fst can_castle;
      queenside_castle = snd can_castle;
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
