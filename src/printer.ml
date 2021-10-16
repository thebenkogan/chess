open Game
open Helper

let get_piece_text color = function
  | Pawn -> if color = White then "♟︎" else "♙"
  | Knight -> if color = White then "♞" else "♘"
  | Bishop -> if color = White then "♝" else "♗"
  | Rook -> if color = White then "♜" else "♖"
  | Queen -> if color = White then "♛" else "♕"
  | King -> if color = White then "♚" else "♔"

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