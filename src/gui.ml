open Graphics
open Images
open Game
open Helper

let img = Images.load "imgs/black_bishop.png" []

let load_imgs () =
  List.map Graphic_image.of_image
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
    ]

let windowlength = 600

let step = windowlength / 8

let rec draw_rows row =
  if row = 8 then ()
  else
    let rec draw_row index =
      draw_rect (index * step) (row * step) step step;
      if index = 7 then () else draw_row (index + 1)
    in
    draw_row 0;
    draw_rows (row + 1)

let draw_board () = draw_rows 0

let get_piece_img imgs color soldier =
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

let rec draw_position_rows bd imgs row =
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

let draw_position bd imgs =
  let bd = board_to_array bd in
  draw_position_rows bd imgs 0

let draw_game bd =
  clear_graph ();
  draw_board ();
  let imgs = load_imgs () in
  draw_position bd imgs

let init_gui () =
  open_graph "";
  resize_window windowlength windowlength;
  set_line_width 2