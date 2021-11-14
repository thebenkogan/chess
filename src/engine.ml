open State
open Game

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

let update_states pl opp mv turn =
  let pl' = if turn then play_move pl mv else receive_move pl mv in
  let opp' = if turn then receive_move opp mv else play_move opp mv in
  (pl', opp')

let rec minimax pl opp depth max move first =
  let step acc mv =
    let pl', opp' = update_states pl opp mv max in
    let choose = if max then Stdlib.max else Stdlib.min in
    let mv = if first then mv else move in
    choose acc (minimax pl' opp' (depth - 1) (not max) mv false)
  in
  if depth = 0 then (eval pl.game_state.board pl.game_state.color, move)
  else if max then List.fold_left step (neg_infinity, move) pl.moves
  else List.fold_left step (infinity, move) opp.moves

(*let next_move pl opp = let eval_move mv = print_endline "Evaluated one
  move"; let pl', opp' = update_states pl opp mv true in minimax pl'
  opp' 2 false in let evals = List.map eval_move pl.moves in let map =
  List.combine evals pl.moves in List.sort compare map |> List.rev |>
  List.hd |> snd*)

let next_move pl opp =
  snd (minimax pl opp 3 true ((-1, -1), (-1, -1)) true)
