val next_move : State.t -> State.t -> Game.move
(** [next_move pl opp] is the next move for [pl] playing against [opp].
    Requires: [pl] has moves to play in the current position (not in
    checkmate/stalemate). *)
