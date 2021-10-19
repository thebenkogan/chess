val pretty_print : Game.t -> unit
(** [pretty_print board] prints the terminal version of the [board]. The
    board is drawn from white's perspective. Requires: [board] is a
    valid chess board. *)

val pp_move_list : Game.move list -> string
(** [pp_move_list lst] pretty-prints list [lst], using [pp_move] to
    pretty-print each move. TAKEN FROM A2.*)
