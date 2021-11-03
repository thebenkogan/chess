open Chess
open State
open Game
open Boards
open Helper
open Printer
open Gui

exception Illegal

type game_result = Win of color | Draw 

(** [is_checkmate st] is true if [st] is currently in checkmate. A state
    is in checkmate if it has no legal moves and its king is in check. *)
let is_checkmate st = List.length st.moves = 0 && st.king_in_check

(** [is_stalemate st] is true if [st] is currently in stalemate. A state
    is in stalemate if it has no legal moves. *)
let is_stalemate st = List.length st.moves = 0

(** [play_game state black result] draws the current state of the game
    onto the Graphics window and handles white's [state] in the current
    game by receiving a clicked move. If [result] specifies a color,
    then that color wins by checkmate. After the player inputs a move,
    this checks if the move is legal. If illegal, this repeats with no
    new inputs. If legal, the move is played, and then this chooses a
    random move based off of black's state after updating [black] with
    the new move. This then repeats with the new states for white and
    black.*)
let rec play_game state black (result : game_result option) =
  match result with
  | Some Win White ->
      (print_endline "\nCheckmate! You win.";
      match draw_win_screen (Some White) with 
        |'p' -> play_game (init_state starting_board White) (init_state starting_board Black) None
        |_ -> exit 0;
      )
  | Some Win Black ->
      (print_endline "\nCheckmate! You Lose.";
      match draw_win_screen (Some Black) with 
        |'p' -> play_game (init_state starting_board White) (init_state starting_board Black) None
        |_ -> exit 0;
      )
  | Some Draw ->
    (print_endline "\nStalemate! Draw.";
      match draw_win_screen None with 
        |'p' -> play_game (init_state starting_board White) (init_state starting_board Black) None
        |_ -> exit 0;
    )
  | None -> (
      let move = draw_game state.game_state.board state.moves in
      try
        if not (List.mem move state.moves) then raise Illegal
        else
          let next_state = play_move state move in
          let next_black = receive_move black move in
          if is_checkmate next_black then
            play_game next_state next_black (Some (Win White))
          else if is_stalemate next_black then 
            play_game next_state next_black (Some Draw)
          else
            let black_move =
              List.nth next_black.moves
                (Random.int (List.length next_black.moves))
            in
            let update_black = play_move next_black black_move in
            let update_state = receive_move next_state black_move in
            if is_checkmate update_state then
              play_game update_state update_black (Some (Win Black))
            else if is_stalemate update_state then 
              play_game next_state next_black (Some Draw)
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
    "\n\
     You can play a move by clicking on a piece and its target square.";
  
  init_gui ();
  Random.self_init ();
  play_game
    (init_state starting_board White)
    (init_state starting_board Black)
    None

(* Execute the game engine. *)
let () = main ()
