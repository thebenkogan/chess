open Chess
open State
open Game
open Boards
open Helper
open Gui
open Engine

type game_result =
  | Win of color
  | Draw

(** [is_checkmate st] is true if [st] is currently in checkmate. A state
    is in checkmate if it has no legal moves and its king is in check. *)
let is_checkmate st =
  List.length st.moves = 0 && st.game_state.king_in_check

(** [is_stalemate st] is true if [st] is currently in stalemate. A state
    is in stalemate if it has no legal moves and its king is not in
    check. *)
let is_stalemate st = List.length st.moves = 0

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
  let next_black = receive_move black move ~promote_piece in
  draw_game_basic next_state.game_state.board;
  if is_checkmate next_black then
    (next_state, next_black, Some (Win White))
  else if is_stalemate next_black then
    (next_state, next_black, Some Draw)
  else
    let black_move = next_move next_black next_state in
    let update_black = play_move next_black black_move in
    let update_state = receive_move next_state black_move in
    if is_checkmate update_state then
      (update_state, update_black, Some (Win Black))
    else if is_stalemate update_state then
      (next_state, next_black, Some Draw)
    else (update_state, update_black, None)

(** [play_game state black result] draws the current state of the game
    onto the Graphics window and handles white's [state] in the current
    game by receiving a clicked move. If [result] specifies a color,
    then that color wins by checkmate. After the player inputs a move,
    this checks if the move is legal. If illegal, this repeats with no
    new inputs. If legal, the move is played, and this repeats with the
    new states for white and black.*)
let rec play_game state black result =
  draw_game_basic state.game_state.board;
  if result <> None then
    let res_color =
      match result with
      | Some (Win White) -> Some White
      | Some (Win Black) -> Some Black
      | _ -> None
    in
    match draw_win_screen res_color with
    | true ->
        play_game
          (init_state starting_board White)
          (init_state starting_board Black)
          None
    | false -> exit 0
  else
    let move = draw_game state.game_state.board state.moves in
    if not (List.mem move state.moves) then play_game state black None
    else
      let update_state, update_black, result =
        play_and_receive state black move
      in
      play_game update_state update_black result

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
  play_game
    (init_state starting_board White)
    (init_state starting_board Black)
    None

(* Execute the game engine. *)
let () = main ()
