type t = {
  game_state : Game.properties;
  moves : Game.move list;
  turn : bool;
}

let init_state (board : Game.t) (color : Game.color) : t =
  raise (Failure "Unimplemented")

let play_move (st : t) (move : Game.move) : t =
  raise (Failure "Unimplemented")
