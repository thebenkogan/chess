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
         Png.load "imgs/grey_square.png" [];
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

let get_potential_squares
    (bd : Game.t)
    (piece : piece)
    (soldier_type : soldier)
    currX
    currY =
  ()

let draw_potential_circles
    (board : Game.t)
    (imgs : image list)
    (currX : int)
    (currY : int)
    (soldier_type : soldier) =
  let potential_circles : move list = [ ((0, 0), (0, 1)) ] in
  (* FINISH THIS LINE THEN DONE *)
  let rec draw_circle potential_circles =
    match potential_circles with
    | [] -> ()
    | ((a, b), (c, d)) :: t ->
        draw_image (List.nth imgs 12)
          ((c * step) + ((step - 60) / 2))
          ((d * step) + ((step - 60) / 2));
        draw_circle t
  in
  draw_circle potential_circles

(** [get_soldier_type piece] is the type of the soldier. Requries:
    [piece] is not None *)
let get_soldier_type (piece : piece option) : soldier =
  match piece with
  | Some (_, Pawn) -> Pawn
  | Some (_, Knight) -> Knight
  | Some (_, Bishop) -> Bishop
  | Some (_, Rook) -> Rook
  | Some (_, Queen) -> Queen
  | _ -> King

(** [draw circles bd imgs] draws grey circles based on the potential
    moves of the clicked square. *)
let draw_circles (bd : Game.t) (imgs : image list) currX currY =
  let board = board_to_array bd in
  let piece = board.(currX).(currY) in
  if piece = None then ()
  else
    (* let soldier_type = get_soldier_type piece in
       draw_potential_circles bd imgs currX currY soldier_type *)
    draw_image (List.nth imgs 12) 0 0

let draw_game (bd : Game.t) =
  clear_graph ();
  draw_board ();
  draw_position bd !imgs;
  (* draw_circles bd !imgs; *)
  let x1, y1 = wait_click_square () in
  let draw_potential = draw_circles bd !imgs x1 y1 in
  draw_potential;
  let x2, y2 = wait_click_square () in
  ((x1, y1), (x2, y2))

let init_gui () =
  open_graph "";
  resize_window window_length window_length;
  set_window_title "OCaml Chess";
  set_line_width 2;
  imgs := load_imgs ()
