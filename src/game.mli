(** Type of soldier fighting for one of the two players of the game. *)
type color =
  | White
  | Black

(** Type of piece on the chess board with unique capabilities. *)
type soldier =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type piece = color * soldier
(** A piece on the chess board. *)

type t = piece option list list
(** 2d list of pieces representing the chess pieces on the board. For a
    piece in the [i]th list and [j]th element in that list, that piece
    is located at [i] [j] on the chess board, where [i] is the file
    starting at the A file and [j] is the rank starting at the 1st rank. *)

type move = (int * int) * (int * int)
(** Chess board coordinate of the old position of the moved piece, and
    the coordinate of the new position of the moved piece. *)

type properties = {
  (*The board with all of the pieces.*)
  board : t;
  (*The color of this player's pieces.*)
  color : color;
  (*The last move played on this board.*)
  last_move : move;
  (*The opponent's legal moves in the current position.*)
  enemy_moves : move list;
  (*True if the starting rook on the A file has moved.*)
  king_moved : bool;
  (*True if the starting rook on the H file has moved.*)
  a_rook_moved : bool;
  (*Add more fields after this as we need them.*)
  h_rook_moved : bool;
}

val update_board : t -> move -> t
(** [update_board board move] is the board with [move] played.*)

val is_attacked : move list -> int * int -> bool
(** [is_attacked enemy_moves coords] is true if the square at [coords]
    is attacked by one or more of the opponent's pieces, specified by
    their legal moves [enemy_moves]. Requires: [coords] is on the board
    and contains either no piece or a piece of the opposite color to
    those for [enemy_moves].*)

val legal_moves : properties -> move list
(** [legal_moves color] is a list of legal moves with [properties]
    providing context to the position and specifying the [color] to
    output moves for.*)

(**[SoldierLogic] defines the interface for each piece to determine the
   legal moves for that piece. Requires: the piece at [coords] is of the
   correct soldier type and is of the same color as specified in
   [properties].*)
module type SoldierLogic = sig
  val legal_moves : properties -> int * int -> move list
end

module Pawn : SoldierLogic

module Knight : SoldierLogic

module Bishop : SoldierLogic

module Rook : SoldierLogic

module Queen : SoldierLogic

module King : SoldierLogic
