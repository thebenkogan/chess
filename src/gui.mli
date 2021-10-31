val init_gui : unit -> unit
(** [init_gui ()] initializes the Graphics GUI by opening a window of
    height and width [window_length], writes the title of the window,
    sets the line width to 2 pixels, and loads the chess piece images
    into [imgs]. *)

val draw_game : Game.t -> Game.move
(** [draw_game bd] redraws the chess board and draws the position in
    [bd]. It then waits for the user to click twice on the Graphics
    window and returns the chess move associated with those clicked
    positions, starting at the first click and moving to the second
    click. Requires: [bd] is a valid chess board. *)

val query_promotion : Game.color -> Game.soldier
(** [query_promotion
  color] is the soldier type the player of [color]
    chooses after prompting them with a menu to select a pawn promotion
    piece. *)
