open Chess
open State
open Game
open Boards
open Helper
open Printer
open Gui

exception Illegal

(** [is_checkmate st] is true if [st] is currently in checkmate. A state
    is in checkmate if it has no legal moves and its king is in check. *)
let is_checkmate st = List.length st.moves = 0 && st.king_in_check

(** [play_game state black result] draws the current state of the game
    onto the Graphics window and handles white's [state] in the current
    game by receiving a clicked move. If [result] specifies a color,
    then that color wins by checkmate. After the player inputs a move,
    this checks if the move is legal. If illegal, this repeats with no
    new inputs. If legal, the move is played, and then this chooses a
    random move based off of black's state after updating [black] with
    the new move. This then repeats with the new states for white and
    black.*)
let rec play_game state black result =
  match result with
  | Some White ->
      print_endline "\nCheckmate! You win.";
      exit 0
  | Some Black ->
      print_endline "\nCheckmate! You Lose.";
      exit 0
  | None -> (
      let move = draw_game state.game_state.board in
      try
        if not (List.mem move state.moves) then raise Illegal
        else
          let next_state = play_move state move in
          let next_black = receive_move black move in
          if is_checkmate next_black then
            play_game next_state next_black (Some White)
          else
            let black_move =
              List.nth next_black.moves
                (Random.int (List.length next_black.moves))
            in
            let update_black = play_move next_black black_move in
            let update_state = receive_move next_state black_move in
            if is_checkmate update_state then
              play_game update_state update_black (Some Black)
            else play_game update_state update_black None
      with Illegal ->
        print_endline "\nIllegal move. Try again.";
        play_game state black None)

(** [main ()] prompts for the game to play, then starts it. The player
    is given the white pieces. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to OCaml Chess!\n\
     You will play with the white pieces against our random-move engine.\n";
  print_endline
    "You can play a move by entering the starting and ending\n\
     coordinate of the piece you want to play with the format:\n";
  print_endline "((current x, current y), (new x, new y))\n";
  print_endline
    "Both x and y are 0-based and are relative to the bottom left of\n\
     the board. Example for starting position of white king: (4, 0).\n";

  print_endline
    "A list of your legal moves will be displayed for every position.\n";

  print_endline "Enter \"quit\" to stop playing.\n\n";
  print_endline "Enter any key to start.\n";
  print_string "> ";
  match read_line () with
  | _ ->
      init_gui ();
      Random.self_init ();
      play_game
        (init_state starting_board White)
        (init_state starting_board Black)
        None

(* Execute the game engine. *)
let () = main ()
