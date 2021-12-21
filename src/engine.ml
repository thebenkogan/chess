open State
open Game

let depth = 4

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
  | [], _ when pl.game_state.king_in_check && max -> Some neg_infinity
  | [], _ when (not pl.game_state.king_in_check) && max -> Some 0.
  | _, [] when opp.game_state.king_in_check && not max -> Some infinity
  | _, [] when (not opp.game_state.king_in_check) && not max -> Some 0.
  | _, _ -> None

let eval_move (move : move) pl opp =
  let board_arr = Helper.board_to_array pl.game_state.board in
  let color = pl.game_state.color in
  let capturePieceType = board_arr.(fst (snd move)).(snd (snd move)) in
  let movePieceType = board_arr.(fst (fst move)).(snd (fst move)) in
  let piece_value = function
    | Some (c, Pawn) -> if c = color then 10. else -10.
    | Some (c, Knight) -> if c = color then 30. else -30.
    | Some (c, Bishop) -> if c = color then 30. else -30.
    | Some (c, Rook) -> if c = color then 50. else -50.
    | Some (c, Queen) -> if c = color then 90. else -90.
    | _ -> 0.
  in
  let valuable_pieces =
    if capturePieceType != None then
      (10. *. piece_value capturePieceType) -. piece_value movePieceType
    else 0.
  in
  let pawn_promotion =
    if is_pawn_promotion pl.game_state.board move then 90. else 0.
  in
  let king_attack =
    if is_attacked [ move ] opp.game_state.king_pos then 10. else 0.
  in
  let enemy_pawn_attack =
    let is_attacked_by_pawn
        (enemy_moves : move list)
        (coords : int * int) : bool =
      let rec pawn_targets t =
        match enemy_moves with
        | [] -> []
        | mv :: t ->
            if
              Some (opp.game_state.color, Pawn)
              == board_arr.(fst (fst mv)).(snd (fst mv))
            then mv :: pawn_targets t
            else pawn_targets t
      in
      List.mem move (pawn_targets opp.moves)
    in
    if is_attacked_by_pawn opp.moves (fst move) then
      -.piece_value movePieceType
    else 0.
  in
  valuable_pieces +. pawn_promotion +. king_attack +. enemy_pawn_attack

let rec remove_value_and_sort moves =
  let compare_values value1 value2 =
    if value1 == value2 then 0 else if value1 > value2 then 1 else -1
  in
  let sorted_values = List.sort compare_values moves in
  match sorted_values with
  | [] -> []
  | (value, mv) :: t -> mv :: remove_value_and_sort t

let rec order_with_value moves pl opp =
  match moves with
  | [] -> []
  | mv :: t -> (eval_move mv pl opp, mv) :: order_with_value t pl opp

let rec order_moves (moves : move list) pl opp =
  match remove_value_and_sort (order_with_value moves pl opp) with
  | [] -> []
  | mv :: t -> mv :: order_moves t pl opp

(** [minimax pl opp depth max move first] is maximizing move for [pl] if
    [max] or the minimizing move for [pl] if [not max], searching to
    [depth]. [move] is the first move played by [pl] in some path, with
    this move determined when [first] is true. *)
let rec minimax pl opp depth max (alpha, beta) =
  counter := !counter + 1;
  let rec step value (a, b) = function
    | [] -> value
    | mv :: t ->
        let pl', opp' = update_states pl opp mv max in
        let choose = if max then Stdlib.max else Stdlib.min in
        let value' =
          choose value (minimax pl' opp' (depth - 1) (not max) (a, b))
        in
        if (max && value' >= b) || ((not max) && value' <= a) then
          value'
        else
          let a' = if max then choose a value' else a in
          let b' = if not max then choose b value' else b in
          step value' (a', b') t
  in
  match result pl opp max with
  | Some v -> v
  | None ->
      if depth = 0 then eval pl.game_state.board pl.game_state.color
      else if max then
        step neg_infinity (alpha, beta) (order_moves pl.moves pl opp)
      else step infinity (alpha, beta) (order_moves opp.moves opp pl)

let next_move pl opp =
  counter := 1;
  let rec eval_moves best_value best_move = function
    | [] ->
        print_endline (string_of_int !counter);
        best_move
    | mv :: t ->
        let pl', opp' = update_states pl opp mv true in
        let eval =
          minimax pl' opp' (depth - 1) false (best_value, infinity)
        in
        let best_value' =
          if eval > best_value then eval else best_value
        in
        let best_move' = if eval > best_value then mv else best_move in
        eval_moves best_value' best_move' t
  in
  eval_moves neg_infinity (List.hd pl.moves) pl.moves