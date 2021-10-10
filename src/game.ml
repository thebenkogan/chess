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
  king_pos : int * int;
  king_in_check : bool;
  kingside_castle : bool;
  queenside_castle : bool;
}

let update_board (bd : t) (mv : move) : t =
  raise (Failure "Unimplemented")

let is_attacked (enemy_moves : move list) (coords : int * int) : bool =
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
  val legal_moves :
    properties ->
    int * int ->
    (properties -> int * int -> bool) ->
    move list
end

module Pawn : SoldierLogic = struct
  let legal_moves (prop : properties) (coords : int * int) pin_checker :
      move list =
    if pin_checker prop coords then []
    else raise (Failure "Unimplemented")
end

module Knight : SoldierLogic = struct
  (** [potential_squares coords board_arr color] are all of the
      potential moves for the knight located at [coords] on the board
      array [board_arr] with color [color]. A potential square is one
      that is on the board and does not contain a piece of the same
      color. *)
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
  let legal_moves (prop : properties) (coords : int * int) pin_checker :
      move list =
    if pin_checker prop coords then []
    else
      let board = board_to_array prop.board in
      squares_to_moves coords
        (potential_squares coords board prop.color)
end

module Bishop : SoldierLogic = struct
  let legal_moves (prop : properties) (coords : int * int) pin_checker :
      move list =
    if pin_checker prop coords then []
    else raise (Failure "Unimplemented")
end

module Rook : SoldierLogic = struct
  let legal_moves (prop : properties) (coords : int * int) pin_checker :
      move list =
    if pin_checker prop coords then []
    else raise (Failure "Unimplemented")
end

module Queen : SoldierLogic = struct
  let legal_moves (prop : properties) (coords : int * int) pin_checker :
      move list =
    if pin_checker prop coords then []
    else raise (Failure "Unimplemented")
end

module King : SoldierLogic = struct
  (** [potential_squares coords board_arr color] are all of the
      potential moves for the king located at [coords] on the board
      array [board_arr] with color [color]. A potential square is one
      that is on the board and does not contain a piece of the same
      color. *)
  let potential_squares (x, y) board_arr color =
    List.filter
      (is_valid_square board_arr color)
      [
        (x + 1, y);
        (x - 1, y);
        (x, y + 1);
        (x, y - 1);
        (x + 1, y + 1);
        (x + 1, y - 1);
        (x - 1, y + 1);
        (x - 1, y - 1);
      ]

  (** [castle_squares prop coords] is the list of squares to which the
      king can currently castle as specified by [prop] and the king's
      current position [coords]. If the king cannot castle kingside or
      queenside, this is the empty list. *)
  let castle_squares (prop : properties) (x, y) =
    match (prop.kingside_castle, prop.queenside_castle) with
    | true, true -> [ (x + 2, y); (x - 2, y) ]
    | true, false -> [ (x + 2, y) ]
    | false, true -> [ (x - 2, y) ]
    | false, false -> []

  let legal_moves (prop : properties) (coords : int * int) _ : move list
      =
    let board = board_to_array prop.board in
    let castle_append = castle_squares prop coords in
    let not_attacked square =
      not (is_attacked prop.enemy_moves square)
    in
    squares_to_moves coords
      (castle_append
      @ List.filter not_attacked
          (potential_squares coords board prop.color))
end

(* ASSUMPTION FOR THE FOLLOWING FUNCTIONS: A board is a 2d list of
   pieces as defined above, where the first element in that list is the
   first column on the chess board (column 0, leftmost). Each element in
   a corresponding column is ordered by row starting at 0. We get the
   legal moves for each piece of our color by iterating through every
   column and getting the legal moves for the elements in those columns
   that match the color defined in prop, then concatenating the results
   together. *)

(** [moves_for_column prop (x, y) pin_checker column] is the list of all
    legal moves for each piece in column number [x] that is of the same
    color as defined in [prop], beginning at the [y]th element of the
    column. If a piece is considered pinned by [pin_checker], then no
    moves will be returned for that piece. Requires: [x] and [y] are in
    0..7.*)
let rec moves_for_column prop (x, y) pin_checker = function
  | [] -> []
  | None :: t -> moves_for_column prop (x, y + 1) pin_checker t
  | Some (color, soldier) :: t ->
      if color <> prop.color then
        moves_for_column prop (x, y + 1) pin_checker t
      else
        begin
          begin
            match soldier with
            | Pawn -> Pawn.legal_moves prop (x, y) pin_checker
            | Knight -> Knight.legal_moves prop (x, y) pin_checker
            | Bishop -> Bishop.legal_moves prop (x, y) pin_checker
            | Rook -> Rook.legal_moves prop (x, y) pin_checker
            | Queen -> Queen.legal_moves prop (x, y) pin_checker
            | King -> King.legal_moves prop (x, y) pin_checker
          end
          @ moves_for_column prop (x, y + 1) pin_checker t
        end

let legal_moves ?pin_checker:(pc = fun _ _ -> false) (prop : properties)
    =
  let column_handler (lst, col_num) column =
    (lst @ moves_for_column prop (col_num, 0) pc column, col_num + 1)
  in
  fst (List.fold_left column_handler ([], 0) prop.board)

(*Can only test this when legal_moves is complete.*)
let pin_checker (prop : properties) (x, y) : bool =
  let board_arr = board_to_array prop.board in
  board_arr.(x).(y) <- None;
  let new_board = array_to_board board_arr in
  let enemy_color = if prop.color = White then Black else White in
  is_attacked
    (legal_moves
       {
         prop with
         board = new_board;
         color = enemy_color;
         king_in_check = false;
       })
    prop.king_pos
