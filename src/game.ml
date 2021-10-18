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

(** [update_board bd mv] updates the board [bd] by moving the piece
    according to [mv] by its new location. Requires: [mv] represents a
    complete and legally valid game move *)
let update_board (bd : t) (((old_x, old_y), (new_x, new_y)) : move) : t
    =
  let board_arr = board_to_array bd in
  let old_piece = board_arr.(old_x).(old_y) in
  board_arr.(old_x).(old_y) <- None;
  board_arr.(new_x).(new_y) <- old_piece;
  let check_conditions =
    (* Check for if pawn reaches either end *)
    if
      board_arr.(new_x).(new_y) = Some (White, Pawn)
      && new_y = 7 && old_y < 7
    then board_arr.(new_x).(new_y) <- Some (White, Queen)
    else if
      board_arr.(new_x).(new_y) = Some (Black, Pawn)
      && new_y = 0 && old_y > 0
    then board_arr.(new_x).(new_y) <- Some (Black, Queen);
    (* Check for if en passant just occurred *)
    if
      old_piece = Some (White, Pawn)
      && (new_x - old_x = 1 || new_x - old_x = -1)
      && new_y - old_y = 1
    then board_arr.(new_x + (new_x - old_x)).(new_y - 1) <- None
    else if
      old_piece = Some (Black, Pawn)
      && (new_x - old_x = 1 || new_x - old_x = -1)
      && new_y - old_y = -1
    then board_arr.(new_x + (new_x - old_x)).(new_y + 1) <- None;
    (* Check if castling just occurred *)
    (* Rightside castle *)
    if
      (old_piece = Some (White, King) || old_piece = Some (Black, King))
      && new_x - old_x = 2
    then board_arr.(new_x - 1).(new_y) <- board_arr.(7).(new_y);
    board_arr.(7).(new_y) <- None;
    (* Leftside castle *)
    if
      (old_piece = Some (White, King) || old_piece = Some (Black, King))
      && new_x - old_x = -2
    then board_arr.(new_x + 1).(new_y) <- board_arr.(0).(new_y);
    board_arr.(0).(new_y) <- None
  in
  check_conditions;
  let output_board = array_to_board board_arr in
  output_board

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
    (properties -> move -> bool) ->
    move list
end

module Pawn : SoldierLogic = struct
  (** Returns true if the previous move is en passant-able (i.e. moved
      two from the previous y-position), and that the current coordinate
      position of the pawn can move diagonally due to en passant rule *)

  let check_en_passant_left
      (curr_x, curr_y)
      ((last_x_old, last_y_old), (last_x_new, last_y_new))
      color
      board =
    let last_piece = board.(last_x_new).(last_y_new) in
    let net_y = last_y_new - last_y_old in

    if color = White then
      if
        (* Looking diagonally up-left *)
        last_piece = Some (Black, Pawn)
        && net_y = -2
        && last_x_new - curr_x = -1
        && board.(curr_x - 1).(curr_y + 1) = None
      then true
      else false
    else if
      (* Looking diagonally down-left *)
      last_piece = Some (White, Pawn)
      && net_y = 2
      && last_x_new - curr_x = -1
      && board.(curr_x - 1).(curr_y - 1) = None
    then true
    else false

  let check_en_passant_right
      (curr_x, curr_y)
      ((last_x_old, last_y_old), (last_x_new, last_y_new))
      color
      board =
    let last_piece = board.(last_x_new).(last_y_new) in
    let net_y = last_y_new - last_y_old in

    if color = White then
      if
        last_piece = Some (Black, Pawn)
        && net_y = -2
        && last_x_new - curr_x = 1
        && board.(curr_x - 1).(curr_y + 1) = None
      then true
      else false
    else if
      last_piece = Some (White, Pawn)
      && net_y = 2
      && last_x_new - curr_x = 1
      && board.(curr_x - 1).(curr_y - 1) = None
    then true
    else false

  let is_valid_square_pawn
      (curr_x, curr_y)
      board
      color
      last_move
      (pot_x, pot_y) : bool =
    let net_x = pot_x - curr_x in
    let net_y = pot_y - curr_y in
    let left_en_passant_able =
      check_en_passant_left (curr_x, curr_y) last_move color board
    in
    let right_en_passant_able =
      check_en_passant_right (curr_x, curr_y) last_move color board
    in
    let basic_valid_square =
      is_valid_square board color (pot_x, pot_y)
    in

    let check_conditions =
      if color = White then
        if
          (* Go forward two *)
          basic_valid_square && curr_y = 1 && net_y = 2 && net_x = 2
          && board.(curr_x).(curr_y + 1) = None
          && board.(curr_x).(curr_y + 2) = None
        then true (* Go forward one *)
        else if
          basic_valid_square && net_y = 1 && net_x = 0
          && board.(curr_x).(curr_y + 1) = None
        then true (* Diagonal up-left -- no en passant *)
        else if
          basic_valid_square
          && (not left_en_passant_able)
          && net_y = 1 && net_x = -1
          && board.(curr_x - 1).(curr_y + 1) != None
        then true (* Diagonal up-left -- Yes en passant *)
        else if
          basic_valid_square && left_en_passant_able && net_y = 1
          && net_x = -1
          && board.(curr_x - 1).(curr_y + 1) = None
        then true (* Diagonal up-right -- no en passant *)
        else if
          basic_valid_square
          && (not right_en_passant_able)
          && net_y = 1 && net_x = 1
          && board.(curr_x + 1).(curr_y + 1) != None
        then true (* Diagonal up-right -- Yes en passant *)
        else if
          basic_valid_square && right_en_passant_able && net_y = 1
          && net_x = 1
          && board.(curr_x + 1).(curr_y + 1) = None
        then true
        else false (* Check when color is Black *)
      else if
        (* Go forward two *)
        basic_valid_square && curr_y = 6 && net_y = -2 && net_x = 0
        && board.(curr_x).(curr_y - 1) = None
        && board.(curr_x).(curr_y - 2) = None
      then true (* Go forward one *)
      else if
        basic_valid_square && net_y = -1 && net_x = 0
        && board.(curr_x).(curr_y - 1) = None
      then true (* Diagonal down-left -- no en passant *)
      else if
        basic_valid_square
        && (not left_en_passant_able)
        && net_y = -1 && net_x = -1
        && board.(curr_x - 1).(curr_y - 1) != None
      then true (* Diagonal down-left -- Yes en passant *)
      else if
        basic_valid_square && left_en_passant_able && net_y = -1
        && net_x = -1
        && board.(curr_x - 1).(curr_y - 1) = None
      then true (* Diagonal down-right -- no en passant *)
      else if
        basic_valid_square
        && (not right_en_passant_able)
        && net_y = -1 && net_x = 1
        && board.(curr_x + 1).(curr_y - 1) != None
      then true (* Diagonal down-right -- Yes en passant *)
      else if
        basic_valid_square && right_en_passant_able && net_y = -1
        && net_x = 1
        && board.(curr_x + 1).(curr_y - 1) = None
      then true
      else false
    in

    check_conditions

  (* =================================================== *)
  let potential_squares
      (curr_x, curr_y)
      board_arr
      (color : color)
      last_move : (int * int) list =
    (* White Pawn potential moves *)
    if color = White then
      List.filter
        (is_valid_square_pawn (curr_x, curr_y) board_arr color last_move)
        [
          (curr_x, curr_y + 2);
          (curr_x, curr_y + 1);
          (curr_x + 1, curr_y + 1);
          (curr_x - 1, curr_y + 1);
        ] (* Black Pawn potential moves *)
    else
      List.filter
        (is_valid_square_pawn (curr_x, curr_y) board_arr color last_move)
        [
          (curr_x, curr_y + 2);
          (curr_x, curr_y + 1);
          (curr_x + 1, curr_y + 1);
          (curr_x - 1, curr_y + 1);
        ]

  let legal_moves
      (prop : properties)
      (coords : int * int)
      pin_checker
      move_checker : move list =
    if pin_checker prop coords then []
    else
      let board_arr = board_to_array prop.board in
      let moves =
        squares_to_moves coords
          (potential_squares coords board_arr prop.color prop.last_move)
      in
      if prop.king_in_check then List.filter (move_checker prop) moves
      else moves
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

  let legal_moves
      (prop : properties)
      (coords : int * int)
      pin_checker
      move_checker : move list =
    if pin_checker prop coords then []
    else
      let board = board_to_array prop.board in
      let moves =
        squares_to_moves coords
          (potential_squares coords board prop.color)
      in
      if prop.king_in_check then List.filter (move_checker prop) moves
      else moves
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

  let legal_moves (prop : properties) (x, y) pin_checker move_checker :
      move list =
    if pin_checker prop (x, y) then []
    else
      let board = board_to_array prop.board in
      let square_list =
        build_diag (x + 1, y + 1) board 1 1 prop.color
        @ build_diag (x - 1, y + 1) board (-1) 1 prop.color
        @ build_diag (x + 1, y - 1) board 1 (-1) prop.color
        @ build_diag (x - 1, y - 1) board (-1) (-1) prop.color
      in
      let moves = squares_to_moves (x, y) square_list in
      if prop.king_in_check then List.filter (move_checker prop) moves
      else moves
end

module Rook : SoldierLogic = struct
  let rec build_row (x, y) board_arr dirx diry color =
    if not (on_board (x, y)) then []
    else
      match board_arr.(x).(y) with
      | None ->
          (x, y)
          :: build_row (x, y) board_arr (x + dirx) (y + diry) color
      | Some (piece_color, _) when piece_color = color -> []
      | Some (_, _) -> [ (x, y) ]

  let legal_moves (prop : properties) (x, y) pin_checker move_checker :
      move list =
    if pin_checker prop (x, y) then []
    else
      let board = board_to_array prop.board in
      let square_list =
        build_row (x + 1, y) board 1 0 prop.color
        @ build_row (x - 1, y) board (-1) 0 prop.color
        @ build_row (x, y + 1) board 0 1 prop.color
        @ build_row (x, y - 1) board 0 (-1) prop.color
      in
      let moves = squares_to_moves (x, y) square_list in
      if prop.king_in_check then List.filter (move_checker prop) moves
      else moves
end

module Queen : SoldierLogic = struct
  let legal_moves
      (prop : properties)
      (coords : int * int)
      pin_checker
      move_checker : move list =
    Bishop.legal_moves prop coords pin_checker move_checker
    @ Rook.legal_moves prop coords pin_checker move_checker
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

  let legal_moves (prop : properties) (coords : int * int) _ _ :
      move list =
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
let rec moves_for_column prop (x, y) pin_checker move_checker = function
  | [] -> []
  | None :: t ->
      moves_for_column prop (x, y + 1) pin_checker move_checker t
  | Some (color, soldier) :: t ->
      if color <> prop.color then
        moves_for_column prop (x, y + 1) pin_checker move_checker t
      else
        begin
          begin
            match soldier with
            | Pawn ->
                Pawn.legal_moves prop (x, y) pin_checker move_checker
            | Knight ->
                Knight.legal_moves prop (x, y) pin_checker move_checker
            | Bishop ->
                Bishop.legal_moves prop (x, y) pin_checker move_checker
            | Rook ->
                Rook.legal_moves prop (x, y) pin_checker move_checker
            | Queen ->
                Queen.legal_moves prop (x, y) pin_checker move_checker
            | King ->
                King.legal_moves prop (x, y) pin_checker move_checker
          end
          @ moves_for_column prop (x, y + 1) pin_checker move_checker t
        end

let legal_moves
    ?pin_checker:(pc = fun _ _ -> false)
    ?move_checker:(mc = fun _ _ -> true)
    (prop : properties) =
  let column_handler (lst, col_num) column =
    (lst @ moves_for_column prop (col_num, 0) pc mc column, col_num + 1)
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

let move_checker (prop : properties) (mv : move) : bool =
  let new_board = update_board prop.board mv in
  let enemy_color = if prop.color = White then Black else White in
  not
    (is_attacked
       (legal_moves
          {
            prop with
            board = new_board;
            color = enemy_color;
            king_in_check = false;
          })
       prop.king_pos)
