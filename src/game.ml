type color =
  | White
  | Black

type soldier =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type piece = color * soldier

type t = piece list

type move = (int * int) * (int * int)

type properties = {
  (*The board with all of the pieces.*)
  board : t;
  (*The color of this player's pieces.*)
  color : color;
  (*The legal moves on the pieces in the board for this color.*)
  moves : move list;
  (*True if the starting rook on the A file has moved.*)
  king_moved : bool;
  (*True if the starting rook on the H file has moved.*)
  a_rook_moved : bool;
  (*Add more fields after this as we need them.*)
  h_rook_moved : bool;
}

let legal_moves prop color = raise (Failure "Unimplemented")

let load_game j = raise (Failure "Unimplemented")

let to_json board = raise (Failure "Unimplemented")
