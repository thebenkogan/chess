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

(** [is_pawn_promotion bd mv] is true if the move [mv] on board [bd] is
    a pawn promotion. A pawn promotion move is when a pawn reaches the
    end of the board in its corresponding direction. Requires: [mv] is a
    legal move in the current position. *)
let is_pawn_promotion (bd : Game.t) ((x1, y1), (x2, y2)) : bool =
  let board_arr = board_to_array bd in
  let white_promote =
    board_arr.(x1).(y1) = Some (White, Pawn) && y2 = 7
  in
  let black_promote =
    board_arr.(x1).(y1) = Some (Black, Pawn) && y2 = 0
  in
  white_promote || black_promote

(** [play_and_receive state black move] is the new states of the player
    and opponent with game result after [move] is played. Prompts the
    player if the move is a pawn promotion. Chooses a random reply for
    the opponent. Returns [(update_state, update_black, result)]:
    [update_state] and [update_black] are the new states for the player
    and opponent respectively after playing and receiving a move, and
    [result] specifies which color wins the game, if any. *)
let play_and_receive state black move =
  let promote_piece =
    if is_pawn_promotion state.game_state.board move then
      query_promotion state.game_state.color
    else Queen
  in
  let next_state = play_move state move ~promote_piece in
  let next_black = receive_move black move in
  if is_checkmate next_black then (next_state, next_black, Some White)
  else
    let black_move =
      List.nth next_black.moves
        (Random.int (List.length next_black.moves))
    in
    let update_black = play_move next_black black_move in
    let update_state = receive_move next_state black_move in
    if is_checkmate update_state then
      (update_state, update_black, Some Black)
    else (update_state, update_black, None)

(** [play_game state black result] draws the current state of the game
    onto the Graphics window and handles white's [state] in the current
    game by receiving a clicked move. If [result] specifies a color,
    then that color wins by checkmate. After the player inputs a move,
    this checks if the move is legal. If illegal, this repeats with no
    new inputs. If legal, the move is played, and this repeats with the
    new states for white and black.*)
let rec play_game state black result =
  match result with
  | Some White -> (
      print_endline "\nCheckmate! You win.";
      match draw_win_screen White with
      | 'p' ->
          play_game
            (init_state starting_board White)
            (init_state starting_board Black)
            None
      | _ -> exit 0)
  | Some Black -> (
      print_endline "\nCheckmate! You Lose.";
      match draw_win_screen Black with
      | 'p' ->
          play_game
            (init_state starting_board White)
            (init_state starting_board Black)
            None
      | _ -> exit 0)
  | None -> (
      let move = draw_game state.game_state.board state.moves in
      try
        if not (List.mem move state.moves) then raise Illegal
        else
          let update_state, update_black, result =
            play_and_receive state black move
          in
          play_game update_state update_black result
      with Illegal -> play_game state black None)

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
