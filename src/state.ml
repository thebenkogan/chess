open Game
open Helper
open Boards

type t = {
  game_state : Game.properties;
  moves : Game.move list;
  enemy_moves : Game.move list;
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
      king_in_check = false;
      kingside_castle = false;
      queenside_castle = false;
    }
  in
  let init_moves =
    if color = White then legal_moves init_properties else []
  in
  {
    game_state = init_properties;
    moves = init_moves;
    enemy_moves = [];
    a_rook_moved = false;
    h_rook_moved = false;
    king_moved = false;
  }

(** [castling_rights king_pos board a_rook_moved h_rook_moved enemy_moves 
    color kingside]
    is true if the side with [color] pieces on [board] can castle in the
    specified direction. [kingside] is true if direction is kingside,
    false is queenside. [a_rook_moved], [h_rook_moved], and
    [enemy_moves] are used to determine legality. *)
let castle_rights_dir
    (king_pos : int * int)
    (board : Game.t)
    (a_rook_moved : bool)
    (h_rook_moved : bool)
    (enemy_moves : move list)
    (color : Game.color)
    (kingside : bool) : bool =
  let dir = if kingside then 1 else -1 in
  let square1 = (fst king_pos + dir, snd king_pos) in
  let square2 = (fst king_pos + (2 * dir), snd king_pos) in
  let square3 = (fst king_pos + (3 * dir), snd king_pos) in
  let board_arr = board_to_array board in
  let piece1 = board_arr.(fst square1).(snd square1) in
  let piece2 = board_arr.(fst square2).(snd square2) in
  let piece3 =
    if kingside then None else board_arr.(fst square3).(snd square3)
  in
  let rook_moved =
    (kingside && h_rook_moved) || ((not kingside) && a_rook_moved)
  in
  piece1 = None && piece2 = None && piece3 = None && (not rook_moved)
  && (not (is_attacked enemy_moves square1))
  && not (is_attacked enemy_moves square2)

(** [castling_rights king_moved king_pos king_in_check h_rook_moved a_rook_moved
    board enemy_moves]
    is the kingside and queenside castling rights for the player.
    Kingside castling is legal if the first element of the output is
    true, and queenside castling is legal if the second element of the
    output is true.

    A player can castle in a certain direction if:

    - The king has not moved, determined by [king_moved], and the
      corresponding rook has not moved, determined by [a_rook_moved] and
      [h_rook_moved].
    - All squares between [king_pos] and rook on [board] are empty.
    - The king is not in check, determined by [king_in_check].
    - All the squares in between [king_pos] and target destination
      (inclusive) are not attacked by [enemy_moves].*)
let castling_rights
    (king_moved : bool)
    (king_pos : int * int)
    (king_in_check : bool)
    (h_rook_moved : bool)
    (a_rook_moved : bool)
    (board : Game.t)
    (enemy_moves : move list)
    (color : Game.color) : bool * bool =
  if king_moved || king_in_check then (false, false)
  else
    ( castle_rights_dir king_pos board a_rook_moved h_rook_moved
        enemy_moves color true,
      castle_rights_dir king_pos board a_rook_moved h_rook_moved
        enemy_moves color false )

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
    king_in_check = false;
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

let play_move
    ?promote_piece:(pp = Queen)
    (st : t)
    ((oldx, oldy), (newx, newy)) : t =
  let new_last_move = ((oldx, oldy), (newx, newy)) in
  let old_board_arr = board_to_array st.game_state.board in
  let old_piece = old_board_arr.(oldx).(oldy) in
  let new_board =
    update_board st.game_state.board new_last_move ~promote_piece:pp
  in
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
      king_in_check = false;
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
    a_rook_moved = new_a_rook_moved;
    h_rook_moved = new_h_rook_moved;
    king_moved = new_king_moved;
  }

let receive_move
    ?promote_piece:(pp = Queen)
    (st : t)
    (((x1, y1), (x2, y2)) as mv : move) : t =
  let new_board =
    update_board st.game_state.board mv ~promote_piece:pp
  in
  let enemy_prop = enemy_properties new_board st.game_state.color in
  let new_enemy_moves = legal_moves enemy_prop in
  let new_king_in_check =
    is_attacked new_enemy_moves st.game_state.king_pos
  in
  let old_board_arr = board_to_array st.game_state.board in
  let rook_captured =
    old_board_arr.(x2).(y2) = Some (st.game_state.color, Rook)
  in
  let new_a_rook_moved =
    st.a_rook_moved
    || (rook_captured && check_a_rook (x2, y2) st.game_state.color)
  in
  let new_h_rook_moved =
    st.h_rook_moved
    || (rook_captured && check_h_rook (x2, y2) st.game_state.color)
  in
  let can_castle =
    castling_rights st.king_moved st.game_state.king_pos
      new_king_in_check st.h_rook_moved st.a_rook_moved new_board
      new_enemy_moves st.game_state.color
  in
  let new_properties =
    {
      st.game_state with
      board = new_board;
      last_move = mv;
      king_in_check = new_king_in_check;
      kingside_castle = fst can_castle;
      queenside_castle = snd can_castle;
    }
  in
  let new_moves =
    legal_moves ~pin_checker ~move_checker new_properties
  in
  {
    st with
    game_state = new_properties;
    moves = new_moves;
    enemy_moves = new_enemy_moves;
    a_rook_moved = new_a_rook_moved;
    h_rook_moved = new_h_rook_moved;
  }
