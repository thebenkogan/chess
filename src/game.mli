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
  (*The current position of this color's king.*)
  king_pos : int * int;
  (*True if the king is attacked in the current position.*)
  king_in_check : bool;
  (*True if the king can castle kingside in the current position.*)
  kingside_castle : bool;
  (*True if the king can castle queenside in the current position.*)
  queenside_castle : bool;
}

val update_board : t -> move -> t
(** [update_board board move] is the board with [move] played.*)

val is_attacked : move list -> int * int -> bool
(** [is_attacked enemy_moves coords] is true if the square at [coords]
    is attacked by one or more of the opponent's pieces, specified by
    their legal moves [enemy_moves]. Requires: [coords] is on the board
    and contains either no piece or a piece of the opposite color to
    those for [enemy_moves].*)

val pin_checker : properties -> int * int -> bool
(** [pin_checker prop coords] is true if the piece at [coords] is
    pinned. A piece is pinned if that color's king is attacked when the
    piece is removed from the board. Requires: [coords] is on the board
    and is a piece of the color specified in [prop].*)

val move_checker : properties -> move -> bool
(** [pin_checker mv] is true if [mv] does not put the king of the side
    specificed by [prop] in check. Requires: [mv] is not a king move.*)

val legal_moves :
  ?pin_checker:(properties -> int * int -> bool) ->
  ?move_checker:(properties -> move -> bool) ->
  properties ->
  move list
(** [legal_moves pin_checker move_checker prop] is a list of legal moves
    with [prop] providing context to the position and specifying the
    color to output moves for. If [pin_checker] is provided, all pieces
    will be checked to see if they are pinned with its logic, and if so,
    will return no legal moves (a piece is pinned if it cannot move
    because the same color king will be attacked). By default,
    [pin_checker] will treat every piece as not pinned. If
    [move_checker] is provided, it will return only the moves of
    unpinned pieces that do not put the king in check. By default,
    [move_checker] will treat every move as not putting the king in
    check.*)

(**[SoldierLogic] defines the interface for each piece to determine the
   legal moves for that piece. Requires: the piece at [coords] is of the
   correct soldier type and is of the same color as specified in
   [properties].*)
module type SoldierLogic = sig
  val legal_moves :
    properties ->
    int * int ->
    (properties -> int * int -> bool) ->
    (properties -> move -> bool) ->
    move list
end

module Pawn : SoldierLogic

module Knight : SoldierLogic

module Bishop : SoldierLogic

module Rook : SoldierLogic

module Queen : SoldierLogic

module King : SoldierLogic
