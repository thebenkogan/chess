type t = {
  game_state : Game.properties;
  moves : Game.move list;
  turn : bool;
}

let init_state board color = raise (Failure "Unimplemented")

let play_move st move = raise (Failure "Unimplemented")
