(** Representation of dynamic chess game.

    This module represents the state of a game of chess as it is being
    played, including the player's current chess position, their legal
    moves, the pseudo-legal enemy moves, the properties of their
    position, and functions that update their state upon a new move from
    this player or their opponent. *)

type t = {
  (*The board with all of the pieces and properties.*)
  game_state : Game.properties;
  (*The legal moves on the pieces in the board for this color.*)
  moves : Game.move list;
  (*The pseudo-legal moves of the opponent in the current position (king
    independent).*)
  enemy_moves : Game.move list;
  (*Players can only make a move if turn is true? After a move is
    played, we set this to false, and back to true when we receive a
    move from the opponent.*)
  turn : bool;
  (*True if the king is attacked in the current position.*)
  king_in_check : bool;
  (*True if the starting rook on the A file has moved.*)
  a_rook_moved : bool;
  (*True if the starting rook on the H file has moved.*)
  h_rook_moved : bool;
  (*True if the king has moved.*)
  king_moved : bool;
}

val init_state : Game.t -> Game.color -> t
(** [init_state board color] is the initial state of the game starting
    with the chess position given by [board] for [color]'s pieces. This
    state includes the legal starting moves if [color] is white and the
    initial conditions. If [color] is black, there are no legal moves. *)

val play_move : ?promote_piece:Game.soldier -> t -> Game.move -> t
(** [play_move st mv] is the state of the chess position with the player
    of [st] playing [mv]. It is responsible for updating the board, last
    move, king position, and if the A rook, H rook, or king have moved.
    All other properties of the [game_state] are kept the same, and
    [moves] is empty. [turn] is set to [false].*)

val receive_move : ?promote_piece:Game.soldier -> t -> Game.move -> t
(** [receive_move st mv] is the state of the chess position with [move]
    played by the opponent of the player of [st]. It is responsible for
    updating the board, last move, enemy moves, determining if the king
    is in check, kingside and queenside castling rights, and the legal
    moves of the new position. All other state variables are kept the
    same. [turn] is set to [true].*)
