open Graphics
open Images
open Game
open Helper

(** [light] is the color of light squares on the board. *)
let light = rgb 236 217 177

(** [dark] is the color of dark squares on the board. *)
let dark = rgb 174 137 94

(** [imgs] is the currently loaded images of the game. *)
let imgs = ref ([] : image list)

(** [make_transparent img] makes any blue color in [img] transparent
    (Graphics.transp). Graphics does not allow loading transparent
    images directly so this must be done manually. Assumes each image in
    [imgs] has blue background that is intended to be transparent. *)
let make_transparent img =
  let image = dump_image img in
  for i = 0 to Array.length image - 1 do
    for j = 0 to Array.length image.(i) - 1 do
      if image.(i).(j) = blue then image.(i).(j) <- transp
      else image.(i).(j) <- image.(i).(j)
    done
  done;
  make_image image

(** [load_imgs ()] loads all chess piece images in ../imgs and returns
    them in a list. Requires: the Graphics window is open. *)
let load_imgs () : image list =
  List.map make_transparent
    (List.map Graphic_image.of_image
       [
         Png.load "imgs/white_pawn.png" [];
         Png.load "imgs/white_knight.png" [];
         Png.load "imgs/white_bishop.png" [];
         Png.load "imgs/white_rook.png" [];
         Png.load "imgs/white_queen.png" [];
         Png.load "imgs/white_king.png" [];
         Png.load "imgs/black_pawn.png" [];
         Png.load "imgs/black_knight.png" [];
         Png.load "imgs/black_bishop.png" [];
         Png.load "imgs/black_rook.png" [];
         Png.load "imgs/black_queen.png" [];
         Png.load "imgs/black_king.png" [];
         Png.load "imgs/green_edges.png" [];
       ])

(** [window_length] is the height and width of the game window in
    pixels. *)
let window_length = 600

(** [step] is the height and width of each square on the chessboard,
    allowing for 8 squares in the x and y direction. *)
let step = window_length / 8

(** [conv coord] converts [coord] to the opposite color perspective. In
    other words, if one were to flip the coordinate system to start at
    the opposite corner, this is the converted coordinate. *)
let conv (x, y) = (7 - x, 7 - y)

(** [click_to_coord coord side] is the chess coordinate from the click
    position [coord] from the perspective of [side]. If the click is
    registered outside the legal chess coordinates, then this is the
    closest chess coordinate to that position.*)
let click_to_coord ((x, y) : int * int) (side : color) =
  let check_bounds = function
    | n when n > 7 -> 7
    | n when n < 0 -> 0
    | n -> n
  in
  let newx = check_bounds (x / step) in
  let newy = check_bounds (y / step) in
  if side = White then (newx, newy) else conv (newx, newy)

(** [get_next_click_pos ()] waits for the user to click on the Graphics
    window and then returns the clicked x, y position relative to the
    bottom left of the window in pixels. *)
let get_next_click_pos () =
  let click = Graphics.wait_next_event [ Button_down ] in
  (click.mouse_x, click.mouse_y)

(** [wait_click_square side] waits for the user to click on the Graphics
    window and then returns the chess coordinate associated with the
    clicked position from the perspective of [side].*)
let wait_click_square (side : color) =
  let pos = get_next_click_pos () in
  click_to_coord pos side

(** [wait_click_promotion ()] waits for the user to click on the
    Graphics window and then returns the selected piece on the pawn
    promotion screen. If the player does not click on a piece, this will
    prompt the player again.*)
let rec wait_click_promotion () =
  match wait_click_square White with
  | 2, 4 -> Knight
  | 3, 4 -> Bishop
  | 4, 4 -> Rook
  | 5, 4 -> Queen
  | _ -> wait_click_promotion ()

(** [wait_click_start ()] waits for the user to click on the Graphics
    window and then returns the selected color of pieces. Assumes the
    start menu is currently drawn on the window. *)
let rec wait_click_start () =
  match wait_click_square White with
  | x, y when x >= 2 && x <= 5 && y = 4 -> White
  | x, y when x >= 2 && x <= 5 && y = 2 -> Black
  | _ -> wait_click_start ()

(** [draw_rows row] draws the outlines of each square on the chess
    board, starting at row number [row] and moving up the board.
    Requires: [row] is in 0..7. *)
let rec draw_rows row start =
  if row = 8 then ()
  else
    let rec draw_row index alt =
      let color = if alt then light else dark in
      set_color color;
      fill_rect (index * step) (row * step) step step;
      if index = 7 then () else draw_row (index + 1) (not alt)
    in
    draw_row 0 start;
    draw_rows (row + 1) (not start)

(** [draw_board ()] draws the outline of each square on the chess board. *)
let draw_board () = draw_rows 0 false

(** [get_piece_img imgs color soldier] is the image of [imgs] associated
    with the piece of color [color] and type [soldier]. Requires: [imgs]
    lists all white piece images, then black piece images, in the order:
    pawn, knight, bishop, rook, queen, king.*)
let get_piece_img
    (imgs : image list)
    (color : Game.color)
    (soldier : Game.soldier) =
  let index =
    match soldier with
    | Pawn -> if color = White then 0 else 6
    | Knight -> if color = White then 1 else 7
    | Bishop -> if color = White then 2 else 8
    | Rook -> if color = White then 3 else 9
    | Queen -> if color = White then 4 else 10
    | King -> if color = White then 5 else 11
  in
  List.nth imgs index

(** [draw_position_rows bd imgs row side] draws the pieces of each row
    of [bd] from the perspective of [side], starting at row number [row]
    and moving up the board. Requires: [row] is in 0..7, [bd] is a valid
    chess board, and [imgs] lists all white piece images, then black
    piece images, in the order: pawn, knight, bishop, rook, queen, king.*)
let rec draw_position_rows
    (bd : piece option array array)
    (imgs : image list)
    (row : int)
    (side : color) =
  if row = 8 then () (* Stops drawing on board *)
  else
    let rec draw_position_row index =
      (match bd.(index).(row) with
      | None -> ()
      | Some (color, soldier) ->
          let index, row =
            if side = White then (index, row) else conv (index, row)
          in
          let img = get_piece_img imgs color soldier in
          draw_image img
            ((index * step) + ((step - 60) / 2))
            ((row * step) + ((step - 60) / 2)));
      if index = 7 then () else draw_position_row (index + 1);
      ()
    in
    draw_position_row 0;
    draw_position_rows bd imgs (row + 1) side;
    ()

(** [draw_position bd imgs side] draws the pieces of [bd] from the
    perspective of [side]. Requires: [bd] is a valid chess board and
    [imgs] lists all white piece images, then black piece images, in the
    order: pawn, knight, bishop, rook, queen, king.*)
let draw_position (bd : Game.t) (imgs : image list) (side : color) =
  let bd = board_to_array bd in
  draw_position_rows bd imgs 0 side

(** [draw_promotion_menu color] draws a pawn promotion menu for the
    player with the [color] pieces. *)
let draw_promotion_menu color =
  set_color white;
  fill_rect (2 * step) (4 * step) (4 * step) (1 * step);
  set_color black;
  draw_rect (2 * step) (4 * step) (4 * step) (1 * step);
  let y_pos = (4 * step) + ((step - 60) / 2) in
  draw_image
    (get_piece_img !imgs color Knight)
    ((2 * step) + ((step - 60) / 2))
    y_pos;
  draw_image
    (get_piece_img !imgs color Bishop)
    ((3 * step) + ((step - 60) / 2))
    y_pos;
  draw_image
    (get_piece_img !imgs color Rook)
    ((4 * step) + ((step - 60) / 2))
    y_pos;
  draw_image
    (get_piece_img !imgs color Queen)
    ((5 * step) + ((step - 60) / 2))
    y_pos;
  ()

let query_promotion (color : Game.color) : Game.soldier =
  draw_promotion_menu color;
  wait_click_promotion ()

(** [get_potential_squares move_list currX currY] returns a (int * int)
    list of potential moves for the location represented by (currX,
    currY). [move_list] reprents all legally valid game moves, [currX]
    is current x-coordinate integer location, [currY] is current
    y-coordinate integer location. *)
let rec get_potential_squares (move_list : move list) (currX, currY) :
    (int * int) list =
  match move_list with
  | ((a, b), (c, d)) :: t when currX = a && currY = b ->
      (c, d) :: get_potential_squares t (currX, currY)
      (* Match currX & currY *)
  | _ :: t ->
      get_potential_squares t (currX, currY)
      (* No match currX or currY *)
  | _ -> []

(** [draw_markers bd side imgs currX currY move_list] draws green edges
    based on the potential moves of the clicked square from the
    perspective of [side]. Requires: [bd] represents a valid board,
    [imgs] is the list of png images to draw, [currX] is current
    x-coordinate integer, [currY] is current y-coordinate integer,
    [move_list] is list of legally valid moves for player. *)
let draw_markers
    (bd : Game.t)
    (side : Game.color)
    (imgs : image list)
    (currX, currY)
    (move_list : move list) =
  let board = board_to_array bd in
  let piece = board.(currX).(currY) in
  let green_color = rgb 0 204 102 in
  if piece = None then ()
  else
    let potential_moves =
      get_potential_squares move_list (currX, currY)
    in
    let get_green_edges = List.nth imgs 12 in
    let rec draw_circle_func (potential_moves : (int * int) list) =
      match potential_moves with
      | (a, b) :: t ->
          let a', b' = if side = White then (a, b) else conv (a, b) in
          if board.(a).(b) = None then (
            set_color green_color;
            fill_circle
              ((a' * step) + (step / 2))
              ((b' * step) + (step / 2))
              12)
          else
            draw_image get_green_edges
              ((a' * step) + ((step - 60) / 2))
              ((b' * step) + ((step - 60) / 2));
          draw_circle_func t
      | [] -> ()
    in
    draw_circle_func potential_moves

(** [draw_start_menu ()] draws the start menu with buttons for choosing
    sides. *)
let draw_start_menu () =
  set_color (rgb 93 93 94);
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color white;
  fill_rect (2 * step) (2 * step) (4 * step) (1 * step);
  fill_rect (2 * step) (4 * step) (4 * step) (1 * step);
  set_color black;
  draw_rect (2 * step) (2 * step) (4 * step) (1 * step);
  draw_rect (2 * step) (4 * step) (4 * step) (1 * step);
  set_font "-*-fixed-medium-r-semicondensed--55-*-*-*-*-*-iso8859-1";
  moveto ((1 * step) + 10) ((6 * step) + 10);
  draw_string "Choose Your Color";
  moveto ((3 * step) + 10) ((4 * step) + 10);
  draw_string "White";
  moveto ((3 * step) + 10) ((2 * step) + 10);
  draw_string "Black"

let draw_game (bd : Game.t) (side : color) (move_list : move list) =
  clear_graph ();
  draw_board ();
  draw_position bd !imgs side;
  let x1, y1 = wait_click_square side in
  let draw_potential = draw_markers bd side !imgs (x1, y1) move_list in
  draw_potential;
  let x2, y2 = wait_click_square side in
  ((x1, y1), (x2, y2))

let draw_start () =
  clear_graph ();
  draw_start_menu ();
  wait_click_start ()

let init_gui () =
  open_graph "";
  resize_window window_length window_length;
  set_window_title "OCaml Chess";
  set_line_width 2;
  imgs := load_imgs ()

let draw_game_basic (bd : Game.t) (side : color) =
  clear_graph ();
  draw_board ();
  draw_position bd !imgs side

(** [wait_action ()] waits a user input of a key and will continue to be
    called until one of the presented options is pressed. *)
let rec wait_action () =
  match read_key () with
  | 'p' | 'P' -> true
  | 'q' | 'Q' -> false
  | _ -> wait_action ()

let draw_win_screen (result : bool option) =
  set_color white;
  fill_rect (window_length / 8) (window_length / 3)
    (window_length * 3 / 4)
    (window_length / 3);
  set_color black;
  draw_rect (window_length / 8) (window_length / 3)
    (window_length * 3 / 4)
    (window_length / 3);
  set_font "-*-fixed-medium-r-semicondensed--19-*-*-*-*-*-iso8859-1";
  moveto (window_length / 6) (window_length / 2);
  if result = Some true then
    draw_string "You win! Press P to play again, Q to quit"
  else if result = Some false then
    draw_string "You Lose! Press P to play again, Q to quit"
  else draw_string "Stalemate! Press P to play again, Q to quit";
  wait_action ()
