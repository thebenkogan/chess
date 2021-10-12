open Chess
open State
open Game
open Boards
open Helper

let get_piece_text color = function
  | Pawn -> if color = White then "♟︎" else "♙"
  | Knight -> if color = White then "♞" else "♘"
  | Bishop -> if color = White then "♝" else "♗"
  | Rook -> if color = White then "♜" else "♖"
  | Queen -> if color = White then "♛" else "♕"
  | King -> if color = White then "♚" else "♔"

let rec row_major arr bd row =
  if row = 8 then ()
  else
    let rec build_row index =
      (match bd.(index).(row) with
      | None -> arr.(row).(index) <- " "
      | Some (piece_color, soldier_type) ->
          arr.(row).(index) <- get_piece_text piece_color soldier_type);
      if index = 7 then () else build_row (index + 1);
      ()
    in
    build_row 0;
    row_major arr bd (row + 1);
    ()

let rec print_board board row =
  print_endline "";
  print_string "|";
  if row = -1 then ()
  else
    let rec print_row index =
      print_string (board.(row).(index) ^ " |");
      if index = 7 then () else print_row (index + 1);
      ()
    in
    print_row 0;
    print_board board (row - 1);
    ()

let pretty_print () bd =
  let bd = board_to_array bd in
  let board = Array.make_matrix 8 8 "" in
  row_major board bd 0;
  print_board board 7

let () = pretty_print () starting_board

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game f = raise (Failure "Unimplemented")

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline
    "Please enter the name of the game file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json")

(* Execute the game engine. *)
let () = main ()
