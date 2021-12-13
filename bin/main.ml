open Chess
open State
open Game
open Boards
open Helper
open Gui
open Engine

type game_result =
  | Win
  | Loss
  | Draw

(** [is_checkmate st] is true if [st] is currently in checkmate. A state
    is in checkmate if it has no legal moves and its king is in check. *)
let is_checkmate st =
  List.length st.moves = 0 && st.game_state.king_in_check

(** [is_stalemate st] is true if [st] is currently in stalemate. A state
    is in stalemate if it has no legal moves and its king is not in
    check. *)
let is_stalemate st = List.length st.moves = 0

let get_init_states = function
  | White ->
      (init_state starting_board White, init_state starting_board Black)
  | Black ->
      let pl, opp =
        ( init_state starting_board Black,
          init_state starting_board White )
      in
      let opp_move = next_move opp pl in
      let opp' = play_move opp opp_move in
      let pl' = receive_move pl opp_move in
      (pl', opp')

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
let play_and_receive pl opp move =
  let promote_piece =
    if is_pawn_promotion pl.game_state.board move then
      query_promotion pl.game_state.color
    else Queen
  in
  let next_pl = play_move pl move ~promote_piece in
  let next_opp = receive_move opp move ~promote_piece in
  draw_game_basic next_pl.game_state.board;
  if is_checkmate next_opp then (next_pl, next_opp, Some Win)
  else if is_stalemate next_opp then (next_pl, next_opp, Some Draw)
  else
    let opp_move = next_move next_opp next_pl in
    let update_opp = play_move next_opp opp_move in
    let update_pl = receive_move next_pl opp_move in
    if is_checkmate update_pl then (update_pl, update_opp, Some Loss)
    else if is_stalemate update_pl then (next_pl, next_opp, Some Draw)
    else (update_pl, update_opp, None)

(** [play_game state black result] draws the current state of the game
    onto the Graphics window and handles white's [state] in the current
    game by receiving a clicked move. If [result] specifies a color,
    then that color wins by checkmate. After the player inputs a move,
    this checks if the move is legal. If illegal, this repeats with no
    new inputs. If legal, the move is played, and this repeats with the
    new states for white and black.*)
let rec play_game pl opp result =
  draw_game_basic pl.game_state.board;
  if result <> None then
    let res_color =
      match result with
      | Some Win -> Some pl.game_state.color
      | Some Loss -> Some opp.game_state.color
      | _ -> None
    in
    match draw_win_screen res_color with
    | true ->
        let side = draw_start () in
        draw_game_basic starting_board;
        let pl, opp = get_init_states side in
        play_game pl opp None
    | false -> exit 0
  else
    let move = draw_game pl.game_state.board pl.moves in
    if not (List.mem move pl.moves) then play_game pl opp None
    else
      let update_state, update_black, result =
        play_and_receive pl opp move
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
  let side = draw_start () in
  draw_game_basic starting_board;
  let pl, opp = get_init_states side in
  print_endline (string_of_int (List.length pl.moves));
  play_game pl opp None

(* Execute the game engine. *)
let () = main ()
