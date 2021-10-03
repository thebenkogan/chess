(* NOTE: This file cannot contain any refrences to definitions in
   game.ml as that would cause a dependency cycle. Put any helper
   functions that require that dependency into game.ml. *)

(** [board_to_array board] converts the list representation of a board
    to an array representation, where each column is now an array,
    joined together in a larger array. In other words, this converts a
    2d list to a 2d array.*)
let board_to_array board = Array.of_list (List.map Array.of_list board)

(** [on_board (x, y)] is true if [x] and [y] are in 0..7 inclusive. *)
let on_board (x, y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

(** [same_color (x, y) board color] is true if the piece at [(x, y)] is
    the same color as [color]. Requires: [(x, y)] is a coordinate on the
    board. *)
let same_color (x, y) board color =
  match board.(x).(y) with
  | None -> false
  | Some (piece_color, _) -> piece_color = color

(** [is_valid_square coords board color] is true if [coords] is a valid
    square that a piece can move to. A square is considered valid if it
    is on the board and the piece existing at [coords] is not the same
    color as [color]. Requires: [board] is the array version of the
    board. *)
let is_valid_square board color coords =
  on_board coords && not (same_color coords board color)

(** [squares_to_moves squares first] returns a list of moves starting at
    [start] and targeting each square in [squares].*)
let squares_to_moves first squares =
  let to_move start target = (start, target) in
  List.map (to_move first) squares
