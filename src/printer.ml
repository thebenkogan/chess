open Game
open Helper

(** [get_piece_text color soldier] is the text version of the piece
    specified by [color] and [soldier]. *)
let get_piece_text color = function
  | Pawn -> if color = White then "♟︎" else "♙"
  | Knight -> if color = White then "♞" else "♘"
  | Bishop -> if color = White then "♝" else "♗"
  | Rook -> if color = White then "♜" else "♖"
  | Queen -> if color = White then "♛" else "♕"
  | King -> if color = White then "♚" else "♔"

(** [print_board bd row] iterates through each row of [bd] starting at
    row number [row] and descending towards white's starting position. *)
let rec print_board bd row =
  if row = -1 then ()
  else (
    print_endline "";
    print_string "|";
    let rec print_row index =
      (match bd.(index).(row) with
      | None -> print_string (" " ^ " |")
      | Some (color, soldier) ->
          print_string (get_piece_text color soldier ^ " |"));
      if index = 7 then () else print_row (index + 1);
      ()
    in
    print_row 0;
    print_board bd (row - 1);
    ())

let pretty_print bd =
  let bd = board_to_array bd in
  print_board bd 7

(** [pp_move mv] pretty-prints the move [mv] as it is read as a tuple. *)
let pp_move mv =
  let x1 = string_of_int (fst (fst mv)) in
  let y1 = string_of_int (snd (fst mv)) in
  let x2 = string_of_int (fst (snd mv)) in
  let y2 = string_of_int (snd (snd mv)) in
  "((" ^ x1 ^ ", " ^ y1 ^ "), (" ^ x2 ^ ", " ^ y2 ^ "))"

let pp_move_list lst =
  let pp_elt = pp_move in
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"