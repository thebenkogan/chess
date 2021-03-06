(* NOTE: This file cannot contain any refrences to definitions in
   game.ml as that would cause a dependency cycle. Put any helper
   functions that require that dependency into game.ml. *)

(** [board_to_array board] converts the list representation of a board
    to an array representation, where each column is now an array,
    joined together in a larger array. In other words, this converts a
    2d list to a 2d array.*)
let board_to_array board = Array.of_list (List.map Array.of_list board)

(** [array_to_board board_arr] converts the array representation of a
    board to a list representation, where each column is now a list,
    joined together in a larger list. In other words, this converts a 2d
    array to a 2d list.*)
let array_to_board board_arr =
  Array.to_list (Array.map Array.to_list board_arr)

(** [on_board (x, y)] is true if [x] and [y] are in 0..7 inclusive. *)
let on_board (x, y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

(** [same_color (x, y) board color] is true if the piece at [(x, y)] is
    the same color as [color]. Requires: [(x, y)] is a coordinate on the
    board.*)
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

(** [squares_to_moves first squares] returns a list of moves starting at
    [first] and targeting each square in [squares].*)
let squares_to_moves first squares =
  let to_move start target = (start, target) in
  List.map (to_move first) squares

(** [get_targets moves] is the list of coordinates that each move in
    [moves] targets.*)
let rec get_targets = function
  | [] -> []
  | h :: t -> snd h :: get_targets t

(** [get_piece_targets pos moves] is the list of coordinates for each
    starting position in [moves] that targets [pos]. *)
let rec get_piece_targets pos = function
  | [] -> []
  | h :: t ->
      if snd h = pos then fst h :: get_piece_targets pos t
      else get_piece_targets pos t

(** [build_line (x,y) board_arr dirx diry color] returns a list of valid
    squares in a single direction that a soldier can move to. If moving
    in that direction hits a wall, the last square is before the wall.
    If it hits a same color piece, then the last square is before that
    piece. If it hits a different color piece, then that piece is the
    last square.*)
let rec build_line (x, y) board_arr dirx diry color =
  if not (on_board (x, y)) then []
  else
    match board_arr.(x).(y) with
    | None ->
        (x, y)
        :: build_line (x + dirx, y + diry) board_arr dirx diry color
    | Some (piece_color, _) when piece_color = color -> []
    | Some (_, _) -> [ (x, y) ]

(** [get_piece_moves piece_pos lst] is a list of all the moves in the
    move list [lst] that start at [piece_pos]. *)
let rec get_piece_moves piece_pos = function
  | [] -> []
  | h :: t ->
      if fst h = piece_pos then [ h ] @ get_piece_moves piece_pos t
      else get_piece_moves piece_pos t

(** [in_line p1 p2] is true if [p1] is in line with [p2], meaning one
    can connect the two coordinates with a straight non-diagonal line. *)
let in_line (x1, y1) (x2, y2) = x1 = x2 || y1 = y2

(** [line_moves p1 p2] is a list of moves starting at p1 to all squares
    in a line to [p2] inclusive. Requires: [p1] and [p2] are in line,
    square-diagonally or not. [p1] and [p2] must also be different. *)
let line_moves (x1, y1) (x2, y2) =
  let netx = x2 - x1 in
  let nety = y2 - y1 in
  let dirx = if netx > 0 then 1 else if netx < 0 then -1 else 0 in
  let diry = if nety > 0 then 1 else if nety < 0 then -1 else 0 in
  let rec line start target dirx diry =
    if start = target then [ target ]
    else
      start
      :: line (fst start + dirx, snd start + diry) target dirx diry
  in
  squares_to_moves (x1, y1)
    (line (x1 + dirx, y1 + diry) (x2, y2) dirx diry)
