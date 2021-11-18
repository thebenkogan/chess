open State
open Game
open Printer

let counter = ref 1

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
let result pl opp max =
  match (pl.moves, opp.moves) with
  | [], _ when pl.king_in_check && max -> Some neg_infinity
  | [], _ when (not pl.king_in_check) && max -> Some 0.
  | _, [] when opp.king_in_check && not max -> Some infinity
  | _, [] when (not opp.king_in_check) && not max -> Some 0.
  | _, _ -> None

(** [minimax pl opp depth max move first] is maximizing move for [pl] if
    [max] or the minimizing move for [pl] if [not max], searching to
    [depth]. [move] is the first move played by [pl] in some path, with
    this move determined when [first] is true. *)
let rec minimax pl opp depth max (alpha, beta) =
  counter := !counter + 1;
  let best_move = ref ((-1, -1), (-1, -1)) in
  let rec step value (a, b) = function
    | [] -> (value, !best_move)
    | mv :: t ->
        let pl', opp' = update_states pl opp mv max in
        let choose = if max then Stdlib.max else Stdlib.min in
        let value' =
          choose value
            (fst (minimax pl' opp' (depth - 1) (not max) (a, b)))
        in
        if (max && value' > a) || ((not max) && value' < b) then
          best_move := mv;
        if max && value' >= b then (b, !best_move)
        else if (not max) && value' <= a then (a, !best_move)
        else
          let a' = if max then choose a value' else a in
          let b' = if not max then choose b value' else b in
          step value' (a', b') t
  in
  match result pl opp max with
  | Some v -> (v, ((-1, -1), (-1, -1)))
  | None ->
      if depth = 0 then
        ( eval pl.game_state.board pl.game_state.color,
          ((-1, -1), (-1, -1)) )
      else if max then step neg_infinity (alpha, beta) pl.moves
      else step infinity (alpha, beta) opp.moves

let next_move pl opp =
  counter := 1;
  let out = minimax pl opp 3 true (neg_infinity, infinity) in
  print_endline (string_of_int !counter);
  snd out
