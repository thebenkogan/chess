open State
open Game

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
  | [], _ :: _ when pl.king_in_check && max -> Some neg_infinity
  | [], _ :: _ when (not pl.king_in_check) && max -> Some 0.
  | _ :: _, [] when opp.king_in_check && not max -> Some infinity
  | _ :: _, [] when (not opp.king_in_check) && not max -> Some 0.
  | _, _ -> None

(** [minimax pl opp depth max move first] is maximizing move for [pl] if
    [max] or the minimizing move for [pl] if [not max], searching to
    [depth]. [move] is the first move played by [pl] in some path, with
    this move determined when [first] is true. *)
let rec minimax pl opp depth max move first =
  let step acc mv =
    let pl', opp' = update_states pl opp mv max in
    let choose = if max then Stdlib.max else Stdlib.min in
    let mv = if first then mv else move in
    choose acc (minimax pl' opp' (depth - 1) (not max) mv false)
  in
  match result pl opp max with
  | Some v -> (v, move)
  | None ->
      if depth = 0 then
        (eval pl.game_state.board pl.game_state.color, move)
      else if max then List.fold_left step (neg_infinity, move) pl.moves
      else List.fold_left step (infinity, move) opp.moves

let next_move pl opp =
  snd (minimax pl opp 3 true ((-1, -1), (-1, -1)) true)
