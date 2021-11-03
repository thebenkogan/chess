val draw_game : Game.t -> Game.move list -> Game.move
(** [draw_game bd] redraws the chess board and draws the position in
    [bd]. It then waits for the user to click twice on the Graphics
    window and returns the chess move associated with those clicked
    positions, starting at the first click and moving to the second
    click. Requires: [bd] is a valid chess board. *)

val init_gui : unit -> unit
(** [init_gui ()] initializes the Graphics GUI by opening a window of
    height and width [window_length], writes the title of the window,
    sets the line width to 2 pixels, and loads the chess piece images
    into [imgs]. *)
val draw_win_screen : Game.color option -> char
(** [draw_win_screen result] takes a given result at the end of the game and
    displays the proper message to the user, and gives options for the user
    to either quit or play again. *)