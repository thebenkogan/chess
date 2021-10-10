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

let play_move (st : t) (move : Game.move) : t =
  raise (Failure "Unimplemented")
