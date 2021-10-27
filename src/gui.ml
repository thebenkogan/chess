open Graphics
open Images
open Game
open Helper

let light = rgb 236 217 177

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
let load_imgs () =
  List.map make_transparent
    (List.map Graphic_image.of_image
       [
         Images.load "imgs/white_pawn.png" [];
         Images.load "imgs/white_knight.png" [];
         Images.load "imgs/white_bishop.png" [];
         Images.load "imgs/white_rook.png" [];
         Images.load "imgs/white_queen.png" [];
         Images.load "imgs/white_king.png" [];
         Images.load "imgs/black_pawn.png" [];
         Images.load "imgs/black_knight.png" [];
         Images.load "imgs/black_bishop.png" [];
         Images.load "imgs/black_rook.png" [];
         Images.load "imgs/black_queen.png" [];
         Images.load "imgs/black_king.png" [];
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
  if row = 8 then ()
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

let draw_game (bd : Game.t) =
  clear_graph ();
  draw_board ();
  draw_position bd !imgs;
  print_endline "\nClick first coord: ";
  let x1, y1 = wait_click_square () in
  print_endline "\nClick second coord: ";
  let x2, y2 = wait_click_square () in
  ((x1, y1), (x2, y2))

let init_gui () =
  open_graph "";
  resize_window window_length (window_length + 2);
  set_window_title "OCaml Chess";
  set_line_width 2;
  imgs := load_imgs ()
