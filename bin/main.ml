open Chess
open State
open Game
open Boards
open Helper
open Printer

exception Malformed

exception Illegal

(** [pp_move mv] pretty-prints the move [mv] as it is read as a tuple. *)
let pp_move mv =
  let x1 = string_of_int (fst (fst mv)) in
  let y1 = string_of_int (snd (fst mv)) in
  let x2 = string_of_int (fst (snd mv)) in
  let y2 = string_of_int (snd (snd mv)) in
  "((" ^ x1 ^ ", " ^ y1 ^ "), (" ^ x2 ^ ", " ^ y2 ^ "))"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. TAKEN FROM A2.*)
let pp_list pp_elt lst =
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

(** [input_to_move str] converts [str] to a move. Requires: [str] is the
    string form of a valid move with the correct whitespace and
    parentheses. Format: ((x, y), (x, y)). Raises: [Malformed] if [str]
    does not follow this format. *)
let input_to_move str : move =
  try
    let char_to_int c = int_of_string (Char.escaped c) in
    let curr_x = char_to_int (String.get str 2) in
    let curr_y = char_to_int (String.get str 5) in
    let new_x = char_to_int (String.get str 10) in
    let new_y = char_to_int (String.get str 13) in
    ((curr_x, curr_y), (new_x, new_y))
  with _ -> raise Malformed

(** [play_game state black] prompts the player and handles white's
    [state] in the current game. After the player inputs a move, this
    checks if the move is legal. If illegal, this repeats with no new
    inputs. If legal, the move is played, and then this chooses a random
    move based off of black's state after updating [black] with the new
    move. This then repeats with the new states for white and black. If
    the player enters "quit", this will exit the program. If the input
    is not recognized, this will repeat with no new inputs. Before
    prompting the player, this prints out white's legal moves as a list.*)
let rec play_game state black =
  pretty_print state.game_state.board;
  print_endline ("\n" ^ pp_list pp_move state.moves);
  print_endline "\n Enter a move: ";
  match read_line () with
  | str -> begin
      if str = "quit" then exit 0
      else
        try
          let move = input_to_move str in
          if not (List.mem move state.moves) then raise Illegal
          else
            let next_state = play_move state move in
            let next_black = receive_move black move in
            let black_move =
              List.nth next_black.moves
                (Random.int (List.length next_black.moves))
            in
            let update_black = play_move next_black black_move in
            let update_state = receive_move next_state black_move in
            play_game update_state update_black
        with
        | Malformed -> play_game state black
        | Illegal -> play_game state black
    end

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

  print_endline "Enter \"quit\" to stop playing.\n\n";
  print_endline "Enter any key to start.\n";
  print_string "> ";
  match read_line () with
  | _ ->
      play_game
        (init_state starting_board White)
        (init_state starting_board Black)

(* Execute the game engine. *)
let () = main ()
