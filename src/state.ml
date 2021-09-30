type t = {
  (*The board with all of the pieces and properties.*)
  game_state : Game.properties;
  (*Players can only make a move if turn is true? After a move is
    played, we set this to false, and back to true when we receive a
    move from the opponent.*)
  turn : bool;
}

let init_state board color = raise (Failure "Unimplemented")

let play_move st move = raise (Failure "Unimplemented")
