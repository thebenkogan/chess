(** Representation of dynamic chess game.

    This module represents the state of a game of chess as it is being
    played, including the player's current chess position, their legal
    moves, the properties of their position, and functions that update
    their state upon a new move from this player or their opponent. *)

type t = {
  (*The board with all of the pieces and properties.*)
  game_state : Game.properties;
  (*Players can only make a move if turn is true? After a move is
    played, we set this to false, and back to true when we receive a
    move from the opponent.*)
  turn : bool;
}

val init_state : Game.t -> Game.color -> t
(** [init_state board color] is the initial state of the game starting
    with the chess position given by [board] for [color]'s pieces. This
    state includes the legal starting moves for [color] and the initial
    conditions. Need to think about this. To get the initial legal
    moves, either pass in a specified last move of the game if not at
    the beginning, or pass in a null move otherwise, such as a pawn
    moving in place. *)

val play_move : t -> Game.move -> t
(** [play_move st move] is the state of the chess position with [move]
    played. This can be called for moves by this player and received
    moves from the opponent to update the current board and calculate
    the new moves.*)
