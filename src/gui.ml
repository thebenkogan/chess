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
         Png.load "imgs/green_circle.png" [];
       ])

(** [window_length] is the height and width of the game window in
    pixels. *)
let window_length = 600

(** [step] is the height and width of each square on the chessboard,
    allowing for 8 squares in the x and y direction. *)
let step = window_length / 8

(** [click_to_coord coord] is the chess coordinate from the click
    position [coord]. If the click is registered outside the legal chess
    coordinates, then this is the closest chess coordinate to that
    position.*)
let click_to_coord ((x, y) : int * int) =
  let check_bounds = function
    | n when n > 7 -> 7
    | n when n < 0 -> 0
    | n -> n
  in
  let newx = check_bounds (x / step) in
  let newy = check_bounds (y / step) in
  (newx, newy)

(** [get_next_click_pos ()] waits for the user to click on the Graphics
    window and then returns the clicked x, y position relative to the
    bottom left of the window in pixels. *)
let get_next_click_pos () =
  let click = Graphics.wait_next_event [ Button_down ] in
  (click.mouse_x, click.mouse_y)

(** [wait_click_square ()] waits for the user to click on the Graphics
    window and then returns the chess coordinate associated with the
    clicked position.*)
let wait_click_square () =
  let pos = get_next_click_pos () in
  click_to_coord pos

(** [draw_rows row] draws the outlines of each square on the chess
    board, starting at row number [row] and moving up the board.
    Requires: [row] is in 0..7. *)
let rec draw_rows row start =
  if row = 8 then ()
  else
    let rec draw_row index alt =
      let color = if alt then light else dark in
      set_color color;
      draw_rect (index * step) (row * step) step step;
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

(** [draw_position_rows bd imgs row] draws the pieces of each row of
    [bd], starting at row number [row] and moving up the board.
    Requires: [row] is in 0..7, [bd] is a valid chess board, and [imgs]
    lists all white piece images, then black piece images, in the order:
    pawn, knight, bishop, rook, queen, king.*)
let rec draw_position_rows
    (bd : piece option array array)
    (imgs : image list)
    (row : int) =
  if row = 8 then () (* Stops drawing on board *)
  else
    let rec draw_position_row index =
      (match bd.(index).(row) with
      | None -> ()
      | Some (color, soldier) ->
          let img = get_piece_img imgs color soldier in
          draw_image img
            ((index * step) + ((step - 60) / 2))
            ((row * step) + ((step - 60) / 2)));
      if index = 7 then () else draw_position_row (index + 1);
      ()
    in
    draw_position_row 0;
    draw_position_rows bd imgs (row + 1);
    ()

(** [draw_position_rows bd imgs row] draws the pieces of each row of
    [bd]. Requires: [bd] is a valid chess board and [imgs] lists all
    white piece images, then black piece images, in the order: pawn,
    knight, bishop, rook, queen, king.*)
let draw_position (bd : Game.t) (imgs : image list) =
  let bd = board_to_array bd in
  draw_position_rows bd imgs 0

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

(** [get_green_circle lst] returns the 13th element of lst, which is the
    green circles of potential moves *)
let get_green_circle lst = List.nth lst 13

(** [get_green_circle lst] returns the 12th element of lst, which is the
    green circles of potential moves *)
let get_green_edges lst = List.nth lst 12

(** [draw_potential_edges move_list imgs currX currY] draws the legally
    valid markers onto the board. [move_list] is the legally valid moves
    for the player. [imgs] is the list of png images to draw, [currX] is
    integer of x-coordinate, [currY] is integer of y-coordinate *)
let draw_potential_edges
    (move_list : move list)
    (imgs : image list)
    (currX, currY)
    (board : piece option array array) =
  let potential_moves =
    get_potential_squares move_list (currX, currY)
  in
  let rec draw_circle (potential_moves : (int * int) list) =
    match potential_moves with
    | (a, b) :: t ->
        if board.(a).(b) = None then
          draw_image (get_green_circle imgs)
            ((a * step) + ((step - 60) / 2))
            ((b * step) + ((step - 60) / 2))
        else
          draw_image (get_green_edges imgs)
            ((a * step) + ((step - 60) / 2))
            ((b * step) + ((step - 60) / 2));
        draw_circle t
    | [] -> ()
  in
  draw_circle potential_moves

(** [draw_edges bd imgs currX currY move_list] draws green edges based
    on the potential moves of the clicked square. Requires: [bd]
    represents a valid board, [imgs] is the list of png images to draw,
    [currX] is current x-coordinate integer, [currY] is current
    y-coordinate integer, [move_list] is list of legally valid moves for
    player. *)
let draw_edges
    (bd : Game.t)
    (imgs : image list)
    (currX, currY)
    (move_list : move list) =
  let board = board_to_array bd in
  let piece = board.(currX).(currY) in
  if piece = None then ()
  else draw_potential_edges move_list imgs (currX, currY) board

let draw_game (bd : Game.t) (move_list : move list) =
  clear_graph ();
  draw_board ();
  draw_position bd !imgs;
  let x1, y1 = wait_click_square () in
  let draw_potential = draw_edges bd !imgs (x1, y1) move_list in
  draw_potential;
  let x2, y2 = wait_click_square () in
  ((x1, y1), (x2, y2))

let init_gui () =
  open_graph "";
  resize_window window_length window_length;
  set_window_title "OCaml Chess";
  set_line_width 2;
  imgs := load_imgs ()