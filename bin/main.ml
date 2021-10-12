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

let pretty_print () bd =
  let bd = board_to_array bd in
  print_board bd 7

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
