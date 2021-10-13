open Helper

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

type t = piece option list list

type move = (int * int) * (int * int)

type properties = {
  board : t;
  color : color;
  last_move : move;
  enemy_moves : move list;
  king_moved : bool;
  a_rook_moved : bool;
  h_rook_moved : bool;
}

let update_board (bd : t) (mv : move) : t =
  raise (Failure "Unimplemented")

let is_attacked (enemy_moves : move list) (coords : int * int) =
  let targets = get_targets enemy_moves in
  List.mem coords targets

(**********************************************************************
 * SOLDIER LOGIC:
 **********************************************************************)

(* When implementing this: insert everything specific to the piece's
   logic in its module unexposed. If you suspect some definition will be
   shared by all modules, pull it out of its module and define it in
   this file. *)
module type SoldierLogic = sig
  val legal_moves : properties -> int * int -> move list
end

module Pawn : SoldierLogic = struct
  let legal_moves (prop : properties) (coords : int * int) : move list =
    raise (Failure "Unimplemented")
end

module Knight : SoldierLogic = struct
  let potential_squares (x, y) board_arr color =
    List.filter
      (is_valid_square board_arr color)
      [
        (x + 2, y + 1);
        (x + 2, y - 1);
        (x - 2, y + 1);
        (x - 2, y - 1);
        (x + 1, y + 2);
        (x + 1, y - 2);
        (x - 1, y + 2);
        (x - 1, y - 2);
      ]

  (* Right now, this only returns the knight moves that are on the board
     and do not move to a square with a same color piece on it. *)
  let legal_moves (prop : properties) (coords : int * int) : move list =
    let board = board_to_array prop.board in
    squares_to_moves coords (potential_squares coords board prop.color)
end

module Bishop : SoldierLogic = struct
  let rec build_diag (x, y) board_arr dirx diry color =
    if on_board (x, y) then []
    else
      match board_arr.(x).(y) with
      | None ->
          (x, y)
          :: build_diag (x, y) board_arr (x + dirx) (y + diry) color
      | Some (piece_color, _) when piece_color = color -> []
      | Some (_, _) -> [ (x, y) ]

  let legal_moves (prop : properties) (x, y) : move list =
    let board = board_to_array prop.board in
    let square_list =
      build_diag (x + 1, y + 1) board 1 1 prop.color
      @ build_diag (x - 1, y + 1) board (-1) 1 prop.color
      @ build_diag (x + 1, y - 1) board 1 (-1) prop.color
      @ build_diag (x - 1, y - 1) board (-1) (-1) prop.color
    in
    squares_to_moves (x, y) square_list
end

module Rook : SoldierLogic = struct
  let rec build_row (x, y) board_arr dirx diry color =
    if on_board (x, y) then []
    else
      match board_arr.(x).(y) with
      | None ->
          (x, y)
          :: build_row (x, y) board_arr (x + dirx) (y + diry) color
      | Some (piece_color, _) when piece_color = color -> []
      | Some (_, _) -> [ (x, y) ]

  let legal_moves (prop : properties) (x, y) : move list =
    let board = board_to_array prop.board in
    let square_list =
      build_row (x + 1, y) board 1 0 prop.color
      @ build_row (x - 1, y) board (-1) 0 prop.color
      @ build_row (x, y + 1) board 0 1 prop.color
      @ build_row (x, y - 1) board 0 (-1) prop.color
    in
    squares_to_moves (x, y) square_list
end

module Queen : SoldierLogic = struct
  let legal_moves (prop : properties) (coords : int * int) : move list =
    Bishop.legal_moves prop coords @ Rook.legal_moves prop coords
end

module King : SoldierLogic = struct
  let legal_moves (prop : properties) (coords : int * int) : move list =
    raise (Failure "Unimplemented")
end

(* ASSUMPTION FOR THE FOLLOWING FUNCTIONS: A board is a 2d list of
   pieces as defined above, where the first element in that list is the
   first column on the chess board (column 0, leftmost). Each element in
   a corresponding column is ordered by row starting at 0. We get the
   legal moves for each piece of our color by iterating through every
   column and getting the legal moves for the elements in those columns
   that match the color defined in prop, then concatenating the results
   together. *)

(** [moves_for_row prop (x, y) row] is the list of all legal moves for
    each piece in column number [x] that is of the same color as defined
    in [prop], beginning at the [y]th element of the column. Requires:
    [x] and [y] are in 0..7.*)
let rec moves_for_column prop (x, y) = function
  | [] -> []
  | None :: t -> moves_for_column prop (x, y + 1) t
  | Some (color, soldier) :: t ->
      if color <> prop.color then moves_for_column prop (x, y + 1) t
      else
        begin
          begin
            match soldier with
            | Pawn -> Pawn.legal_moves prop (x, y)
            | Knight -> Knight.legal_moves prop (x, y)
            | Bishop -> Bishop.legal_moves prop (x, y)
            | Rook -> Rook.legal_moves prop (x, y)
            | Queen -> Queen.legal_moves prop (x, y)
            | King -> King.legal_moves prop (x, y)
          end
          @ moves_for_column prop (x, y + 1) t
        end

let legal_moves prop =
  let column_handler (lst, col_num) column =
    (lst @ moves_for_column prop (col_num, 0) column, col_num + 1)
  in
  fst (List.fold_left column_handler ([], 0) prop.board)
