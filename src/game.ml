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
  king_moved : bool;
  a_rook_moved : bool;
  h_rook_moved : bool;
}

let update_board bd mv = raise (Failure "Unimplemented")

let load_game j = raise (Failure "Unimplemented")

(**********************************************************************
 * SOLDIER LOGIC:
 **********************************************************************)

(** [SoldierLogic] defines the interface for each piece to determine the
    legal moves for that piece. Requires: the piece at [coords] is of
    the correct soldier type and is of the same color as specified in
    [properties].

    When implementing this: insert everything specific to the piece's
    logic in its module unexposed. If you suspect some definition will
    be shared by all modules, pull it out of its module and define it in
    this file. *)
module type SoldierLogic = sig
  val legal_moves : properties -> int * int -> move list
end

module Pawn : SoldierLogic = struct
  let legal_moves prop coords = raise (Failure "Unimplemented")
end

module Knight : SoldierLogic = struct
  let legal_moves prop coords = raise (Failure "Unimplemented")
end

module Bishop : SoldierLogic = struct
  let legal_moves prop coords = raise (Failure "Unimplemented")
end

module Rook : SoldierLogic = struct
  let legal_moves prop coords = raise (Failure "Unimplemented")
end

module Queen : SoldierLogic = struct
  let legal_moves prop coords = raise (Failure "Unimplemented")
end

module King : SoldierLogic = struct
  let legal_moves prop coords = raise (Failure "Unimplemented")
end

(* ASSUMPTION FOR THE FOLLOWING FUNCTIONS: A board is a 2d list of
   pieces as defined above, where the first element in that list is the
   first row on the chess board (row 0). Each element in a corresponding
   row is ordered by column starting at 0. We get the legal moves for
   each piece of our color by iterating through every row and getting
   the legal moves for the elements in those rows that match the color
   defined in prop, then concatenating the results together. *)

(** [moves_for_row prop row_num col_num row] is the list of all legal
    moves for each piece in row number [row_num] that is of the same
    color as defined in [prop], beginning at the [col_num]th element of
    the row. Requires: row_num and col_num are in 0..7.*)
let rec moves_for_row prop row_num col_num = function
  | [] -> []
  | None :: t -> moves_for_row prop row_num (col_num + 1) t
  | Some (color, soldier) :: t ->
      if color <> prop.color then
        moves_for_row prop row_num (col_num + 1) t
      else
        begin
          begin
            match soldier with
            | Pawn -> Pawn.legal_moves prop (row_num, col_num)
            | Knight -> Knight.legal_moves prop (row_num, col_num)
            | Bishop -> Bishop.legal_moves prop (row_num, col_num)
            | Rook -> Rook.legal_moves prop (row_num, col_num)
            | Queen -> Queen.legal_moves prop (row_num, col_num)
            | King -> King.legal_moves prop (row_num, col_num)
          end
          @ moves_for_row prop row_num (col_num + 1) t
        end

let legal_moves prop =
  let row_handler (lst, row_num) row =
    (lst @ moves_for_row prop row_num 0 row, row_num + 1)
  in
  fst (List.fold_left row_handler ([], 0) prop.board)
