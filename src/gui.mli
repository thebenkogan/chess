val draw_game : Game.t -> Game.move list -> Game.move -> Game.move
(** [draw_game bd move_list] redraws the chess board and draws the
    position in [bd]. It then waits for the user to click twice on the
    Graphics window and returns the chess move associated with those
    clicked positions, starting at the first click and moving to the
    second click. Highlights the legal moves of the first clicked piece.
    Requires: [bd] is a valid chess board. *)

val query_promotion : Game.color -> Game.soldier
(** [query_promotion color] is the soldier type the player of [color]
    chooses after prompting them with a menu to select a pawn promotion
    piece. *)

val init_gui : unit -> unit
(** [init_gui ()] initializes the Graphics GUI by opening a window of
    height and width [window_length], writes the title of the window,
    sets the line width to 2 pixels, and loads the chess piece images
    into [imgs]. *)

val draw_win_screen : Game.color option -> bool
(** [draw_win_screen result] takes a given result at the end of the game
    and displays the proper message to the user, and gives options for
    the user to either quit or play again. True if the player chooses to
    play again, false if they choose to quit. None corresponds to
    stalemate. *)

val draw_game_basic : Game.t -> unit
(** [draw_game_basic bd] draws the position in [bd]. Requires: [bd] is a
    valid chess board. *)
