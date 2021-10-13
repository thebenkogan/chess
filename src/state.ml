open Game
open Helper

type t = {
  game_state : Game.properties;
  moves : Game.move list;
  turn : bool;
  a_rook_moved : bool;
  h_rook_moved : bool;
  king_moved : bool;
}

let init_state (board : Game.t) (color : Game.color) : t =
  raise (Failure "Unimplemented")

let enemy_properties our_color =
  {
    board = [];
    color = (if our_color = White then Black else White);
    last_move = ((-1, -1), (-1, -1));
    enemy_moves = [];
    king_pos = (-1, -1);
    king_in_check = false;
    queenside_castle = false;
    kingside_castle = false;
  }

let play_move (st : t) ((oldx, oldy), (newx, newy)) : t =
  let move = ((oldx, oldy), (newx, newy)) in
  let old_board_arr = board_to_array st.game_state.board in
  let old_piece = old_board_arr.(oldx).(oldy) in
  let new_board = update_board st.game_state.board move in
  let enemy_prop =
    { (enemy_properties st.game_state.color) with board = new_board }
  in
  let new_enemy_moves = legal_moves in
  let new_king_pos =
    if old_piece = Some (st.game_state.color, King) then (newx, newy)
    else st.game_state.king_pos
  in
  let properties =
    {
      st.game_state with
      board = new_board;
      enemy_moves = [];
      king_pos = new_king_pos;
      king_in_check = false;
    }
  in
  let moves = legal_moves ~pin_checker ~move_checker in
  (*If a piece has moved before, it will stay moved. Otherwise, check if
    this is the first move.*)
  let a_rook_moved =
    st.a_rook_moved
    || (old_piece = Some (st.game_state.color, Rook) && oldx = 0)
  in
  let h_rook_moved =
    st.h_rook_moved
    || (old_piece = Some (st.game_state.color, Rook) && oldx = 7)
  in
  let king_moved =
    st.king_moved
    || (old_piece = Some (st.game_state.color, King) && oldx = 4)
  in
  []
