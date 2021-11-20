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
  king_pos : int * int;
  king_in_check : bool;
  kingside_castle : bool;
  queenside_castle : bool;
}

let update_board
    ?promote_piece:(pp = Queen)
    (bd : t)
    (((old_x, old_y), (new_x, new_y)) : move) : t =
  let board_arr = board_to_array bd in
  let prev_at_loc = board_arr.(new_x).(new_y) in
  let old_piece = board_arr.(old_x).(old_y) in
  board_arr.(old_x).(old_y) <- None;
  board_arr.(new_x).(new_y) <- old_piece;
  let check_conditions =
    (* Check for if pawn reaches either end *)
    if
      board_arr.(new_x).(new_y) = Some (White, Pawn)
      && new_y = 7 && old_y < 7
    then board_arr.(new_x).(new_y) <- Some (White, pp)
    else if
      board_arr.(new_x).(new_y) = Some (Black, Pawn)
      && new_y = 0 && old_y > 0
    then board_arr.(new_x).(new_y) <- Some (Black, pp)
      (* Check for if en passant just occurred *)
    else if
      old_piece = Some (White, Pawn)
      && (new_x - old_x = 1 || new_x - old_x = -1)
      && new_y - old_y = 1
      && prev_at_loc = None
    then board_arr.(new_x).(new_y - 1) <- None
    else if
      old_piece = Some (Black, Pawn)
      && (new_x - old_x = 1 || new_x - old_x = -1)
      && new_y - old_y = -1
      && prev_at_loc = None
    then board_arr.(new_x).(new_y + 1) <- None
      (* Check if castling just occurred *)
      (* Rightside castle *)
    else if
      (old_piece = Some (White, King) || old_piece = Some (Black, King))
      && new_x - old_x = 2
    then
      let move_right =
        board_arr.(new_x - 1).(new_y) <- board_arr.(7).(new_y);
        board_arr.(7).(new_y) <- None
      in
      move_right (* Leftside castle *)
    else if
      (old_piece = Some (White, King) || old_piece = Some (Black, King))
      && new_x - old_x = -2
    then
      let move_left =
        board_arr.(new_x + 1).(new_y) <- board_arr.(0).(new_y);
        board_arr.(0).(new_y) <- None
      in
      move_left
    else ()
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

(**Defines the interface for each piece to determine the legal moves for
   that piece. Requires: the piece at [coords] is of the correct soldier
   type and is of the same color as specified in [properties]. A legal
   move for a piece is one that obeys the rules for the movement of that
   piece and does not put the king in check.*)

module Pawn = struct
  let check_en_passant
      (curr_x, curr_y)
      (((last_x_old, last_y_old), (last_x_new, last_y_new)) : move)
      color
      board =
    (* Check if disregarded last move *)
    if last_x_old = -1 then []
    else
      let enemy_color = if color = White then Black else White in
      let last_piece = board.(last_x_new).(last_y_new) in
      let net_x = last_x_new - curr_x in
      let net_y = last_y_new - last_y_old in
      let en_passant_able : bool =
        (net_y = 2 || net_y = -2)
        && last_y_new = curr_y
        && (net_x = 1 || net_x = -1)
        && last_piece = Some (enemy_color, Pawn)
      in
      if en_passant_able && color = White then
        [ (last_x_new, last_y_new + 1) ]
      else if en_passant_able && color = Black then
        [ (last_x_new, last_y_new - 1) ]
      else []

  let is_valid_square_pawn
      (curr_x, curr_y)
      board
      color
      last_move
      (pot_x, pot_y) : bool =
    let net_x = pot_x - curr_x in
    let net_y = pot_y - curr_y in
    let basic_valid_square =
      is_valid_square board color (pot_x, pot_y)
    in

    let check_conditions =
      if color = White && basic_valid_square then
        if
          (* Go forward two *)
          curr_y = 1 && net_y = 2 && net_x = 0
          && board.(curr_x).(curr_y + 1) = None
          && board.(curr_x).(curr_y + 2) = None
        then true (* Go forward one *)
        else if
          net_y = 1 && net_x = 0 && board.(curr_x).(curr_y + 1) = None
        then true (* Diagonal up-left *)
        else if
          net_y = 1 && net_x = -1
          && board.(curr_x - 1).(curr_y + 1) != None
        then true (* Diagonal up-right *)
        else if
          net_y = 1 && net_x = 1
          && board.(curr_x + 1).(curr_y + 1) != None
        then true
        else false (* Check when color is Black *)
      else if basic_valid_square && color = Black then
        if
          (* Go forward two *)
          curr_y = 6 && net_y = -2 && net_x = 0
          && board.(curr_x).(curr_y - 1) = None
          && board.(curr_x).(curr_y - 2) = None
        then true (* Go forward one *)
        else if
          net_y = -1 && net_x = 0 && board.(curr_x).(curr_y - 1) = None
        then true (* Diagonal down-left -- no en passant *)
        else if
          net_y = -1 && net_x = -1
          && board.(curr_x - 1).(curr_y - 1) != None
        then true (* Diagonal down-right *)
        else if
          net_y = -1 && net_x = 1
          && board.(curr_x + 1).(curr_y - 1) != None
        then true
        else false
      else false
    in

    check_conditions

  (* =================================================== *)
  let potential_squares
      (curr_x, curr_y)
      board_arr
      (color : color)
      (last_move : move) =
    let squares =
      if color = White then
        [
          (curr_x, curr_y + 2);
          (curr_x, curr_y + 1);
          (curr_x + 1, curr_y + 1);
          (curr_x - 1, curr_y + 1);
        ]
      else
        [
          (curr_x, curr_y - 2);
          (curr_x, curr_y - 1);
          (curr_x + 1, curr_y - 1);
          (curr_x - 1, curr_y - 1);
        ]
    in
    let valid_moves =
      List.filter
        (is_valid_square_pawn (curr_x, curr_y) board_arr color last_move)
        squares
    in
    valid_moves
    @ check_en_passant (curr_x, curr_y) last_move color board_arr

  let legal_moves
      (prop : properties)
      (coords : int * int)
      pin_checker
      move_checker : move list =
    let pinned = pin_checker prop Pawn coords <> None in
    let board_arr = board_to_array prop.board in
    let moves =
      squares_to_moves coords
        (potential_squares coords board_arr prop.color prop.last_move)
    in
    if prop.king_in_check || pinned then
      List.filter (move_checker prop) moves
    else moves
end

module Knight = struct
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
    match pin_checker prop Knight coords with
    | Some moves -> moves
    | None ->
        let board = board_to_array prop.board in
        let moves =
          squares_to_moves coords
            (potential_squares coords board prop.color)
        in
        if prop.king_in_check then List.filter (move_checker prop) moves
        else moves
end

module Bishop = struct
  let legal_moves (prop : properties) (x, y) pin_checker move_checker :
      move list =
    match pin_checker prop Bishop (x, y) with
    | Some moves -> moves
    | None ->
        let board = board_to_array prop.board in
        let square_list =
          build_line (x + 1, y + 1) board 1 1 prop.color
          @ build_line (x - 1, y + 1) board (-1) 1 prop.color
          @ build_line (x + 1, y - 1) board 1 (-1) prop.color
          @ build_line (x - 1, y - 1) board (-1) (-1) prop.color
        in
        let moves = squares_to_moves (x, y) square_list in
        if prop.king_in_check then List.filter (move_checker prop) moves
        else moves
end

module Rook = struct
  let legal_moves (prop : properties) (x, y) pin_checker move_checker :
      move list =
    match pin_checker prop Rook (x, y) with
    | Some moves -> moves
    | None ->
        let board = board_to_array prop.board in
        let square_list =
          build_line (x + 1, y) board 1 0 prop.color
          @ build_line (x - 1, y) board (-1) 0 prop.color
          @ build_line (x, y + 1) board 0 1 prop.color
          @ build_line (x, y - 1) board 0 (-1) prop.color
        in
        let moves = squares_to_moves (x, y) square_list in
        if prop.king_in_check then List.filter (move_checker prop) moves
        else moves
end

module Queen = struct
  let legal_moves
      (prop : properties)
      (coords : int * int)
      pin_checker
      move_checker : move list =
    Bishop.legal_moves prop coords pin_checker move_checker
    @ Rook.legal_moves prop coords pin_checker move_checker
end

module King = struct
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

  let legal_moves (prop : properties) (coords : int * int) move_checker
      : move list =
    let board = board_to_array prop.board in
    let castle_append =
      squares_to_moves coords (castle_squares prop coords)
    in
    let moves =
      squares_to_moves coords
        (potential_squares coords board prop.color)
    in
    List.filter (move_checker prop) moves @ castle_append
end

(** [moves_for_column prop (x, y) pc mc column] is the list of all legal
    moves for each piece in column number [x] that is of the same color
    as defined in [prop], beginning at the [y]th element of the column.
    Requires: [x] and [y] are in 0..7.*)
let rec moves_for_column prop (x, y) pc mc = function
  | [] -> []
  | None :: t -> moves_for_column prop (x, y + 1) pc mc t
  | Some (color, soldier) :: t ->
      if color <> prop.color then
        moves_for_column prop (x, y + 1) pc mc t
      else
        begin
          begin
            match soldier with
            | Pawn -> Pawn.legal_moves prop (x, y) pc mc
            | Knight -> Knight.legal_moves prop (x, y) pc mc
            | Bishop -> Bishop.legal_moves prop (x, y) pc mc
            | Rook -> Rook.legal_moves prop (x, y) pc mc
            | Queen -> Queen.legal_moves prop (x, y) pc mc
            | King -> King.legal_moves prop (x, y) mc
          end
          @ moves_for_column prop (x, y + 1) pc mc t
        end

let legal_moves
    ?pin_checker:(pc = fun _ _ _ -> None)
    ?move_checker:(mc = fun _ _ -> true)
    (prop : properties) =
  let column_handler (lst, col_num) column =
    (lst @ moves_for_column prop (col_num, 0) pc mc column, col_num + 1)
  in
  fst (List.fold_left column_handler ([], 0) prop.board)

let pin_checker (prop : properties) (p : soldier) (x, y) :
    move list option =
  if prop.king_in_check then None
  else
    let board_arr = board_to_array prop.board in
    board_arr.(x).(y) <- None;
    let new_board = array_to_board board_arr in
    let enemy_color = if prop.color = White then Black else White in
    let enemy_moves =
      legal_moves
        {
          prop with
          board = new_board;
          color = enemy_color;
          king_in_check = false;
        }
    in
    if not (is_attacked enemy_moves prop.king_pos) then None
    else
      let attack_pos =
        List.hd (get_piece_targets prop.king_pos enemy_moves)
      in
      match p with
      | Pawn -> Some []
      | Knight -> Some []
      | Bishop ->
          if in_line (x, y) attack_pos then Some []
          else Some (line_moves (x, y) attack_pos)
      | Rook ->
          if not (in_line (x, y) attack_pos) then Some []
          else Some (line_moves (x, y) attack_pos)
      | _ -> None

let move_checker (prop : properties) (((x1, y1), (x2, y2)) : move) :
    bool =
  let mv = ((x1, y1), (x2, y2)) in
  let piece = (board_to_array prop.board).(x1).(y1) in
  let king_pos =
    if piece = Some (prop.color, King) then (x2, y2) else prop.king_pos
  in
  let new_board = update_board prop.board mv in
  let enemy_color = if prop.color = White then Black else White in
  not
    (is_attacked
       (legal_moves
          { prop with board = new_board; color = enemy_color })
       king_pos)
