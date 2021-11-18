open State
open Game
open Printer

(** [eval bd color] is the static evaluation of [bd] for the player with
    [color] pieces. Is the difference in material count. *)
let eval bd color =
  let piece_value = function
    | Some (c, Pawn) -> if c = color then 10. else -10.
    | Some (c, Knight) -> if c = color then 30. else -30.
    | Some (c, Bishop) -> if c = color then 30. else -30.
    | Some (c, Rook) -> if c = color then 50. else -50.
    | Some (c, Queen) -> if c = color then 90. else -90.
    | _ -> 0.
  in
  let bd' = List.flatten bd in
  let sum acc p = acc +. piece_value p in
  List.fold_left sum 0. bd'

(** [update_states pl opp mv turn] is the updated states of [pl] and
    [opp] with [mv] played. If [turn], then this move was made by [pl],
    otherwise this move was made by [opp]. *)
let update_states pl opp mv turn =
  let pl' = if turn then play_move pl mv else receive_move pl mv in
  let opp' = if turn then receive_move opp mv else play_move opp mv in
  (pl', opp')

(** [result pl opp max] is the value of the current state of the game
    relative to [pl]. If [pl] is checkmating or stalemating, then this
    is [Some infinity] or [Some 0] respectively. If [opp] is checkmating
    or stalemating, then this is [Some neg_infinity] or [Some 0]
    respectively. [max] determines which side the result is for. *)
let result pl opp max move =
  if move = ((3, 6), (3, 5)) then begin
    print_endline "result check:";
    print_endline (pp_move_list pl.moves);
    print_endline (string_of_bool pl.king_in_check);
    pretty_print pl.game_state.board
  end
  else ();
  match (pl.moves, opp.moves) with
  | [], _ when pl.king_in_check && max ->
      print_endline "gotem!";
      Some neg_infinity
  | [], _ when (not pl.king_in_check) && max -> Some 0.
  | _, [] when opp.king_in_check && not max -> Some infinity
  | _, [] when (not opp.king_in_check) && not max -> Some 0.
  | _, _ -> None

(** [minimax pl opp depth max move first] is maximizing move for [pl] if
    [max] or the minimizing move for [pl] if [not max], searching to
    [depth]. [move] is the first move played by [pl] in some path, with
    this move determined when [first] is true. *)
let rec minimax pl opp depth max (alpha, beta) move first =
  if List.length pl.moves = 0 && max then
    (*pretty_print pl.game_state.board*) ()
  else ();
  let rec step value (a, b) = function
    | [] -> value
    | mv :: t ->
        if first then begin
          print_endline "begin:";
          print_endline (pp_move_list [ mv ]);
          print_endline (string_of_float (fst value));
          print_endline (pp_move_list [ snd value ]);
          print_endline (string_of_float a);
          print_endline (string_of_float b)
        end
        else ();
        if move = ((3, 6), (3, 5)) then begin
          print_endline "begin (3, 6), (3, 5):";
          print_endline (pp_move_list [ mv ]);
          print_endline (string_of_float (fst value));
          print_endline (pp_move_list [ snd value ]);
          print_endline (string_of_float a);
          print_endline (string_of_float b)
        end
        else ();
        let pl', opp' = update_states pl opp mv max in
        let choose = if max then Stdlib.max else Stdlib.min in
        let mv = if first then mv else move in
        let out =
          minimax pl' opp' (depth - 1) (not max) (a, b) mv false
        in
        if first then begin
          print_endline "value:";
          print_endline (string_of_float (fst out))
        end
        else ();
        if move = ((3, 6), (3, 5)) then begin
          print_endline "value:";
          print_endline (string_of_float (fst out))
        end
        else ();
        let value' = choose value out in
        if (max && fst value' >= b) || ((not max) && fst value' <= a)
        then value'
        else
          let a' = if max then choose a (fst value') else a in
          let b' = if not max then choose b (fst value') else b in
          step value' (a', b') t
  in
  match result pl opp max move with
  | Some v -> (v, move)
  | None ->
      if depth = 0 then
        (eval pl.game_state.board pl.game_state.color, move)
      else if max then step (neg_infinity, move) (alpha, beta) pl.moves
      else step (infinity, move) (alpha, beta) opp.moves

let next_move pl opp =
  print_endline "next";
  snd
    (minimax pl opp 2 true
       (neg_infinity, infinity)
       ((-1, -1), (-1, -1))
       true)
