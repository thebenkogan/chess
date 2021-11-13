open State

let eval bd = 0.

let update_states pl opp mv turn =
  let new_pl = if turn then play_move pl mv else receive_move pl mv in
  let new_opp =
    if turn then receive_move opp mv else play_move opp mv
  in
  (new_pl, new_opp)

let rec minimax pl opp depth max =
  let step acc mv =
    let pl', opp' = update_states pl opp mv max in
    let choose = if max then Stdlib.max else Stdlib.min in
    choose acc (minimax pl' opp' (depth - 1) (not max))
  in
  if depth = 0 then eval pl.game_state.board
  else if max then List.fold_left step neg_infinity pl.moves
  else List.fold_left step infinity opp.moves

let next_move pl opp =
  let eval_move mv =
    let pl', opp' = update_states pl opp mv true in
    minimax pl' opp' 5 false
  in
  let evals = List.map eval_move pl.moves in
  let map = List.combine evals pl.moves in
  List.sort compare map |> List.rev |> List.hd |> snd
