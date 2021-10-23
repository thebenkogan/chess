open Graphics

let windowlength = 600

let step = windowlength / 8

let rec draw_rows row start =
  if row = 8 then ()
  else
    let rec draw_row index alt =
      let color = if alt then white else blue in
      set_color color;
      draw_rect (index * step) (row * step) step step;
      fill_rect (index * step) (row * step) step step;
      if index = 7 then () else draw_row (index + 1) (not alt);
      ()
    in
    draw_row 0 start;
    draw_rows (row + 1) (not start);
    ()

let draw_board () = draw_rows 0 false

let draw_stuff () =
  open_graph "";
  resize_window windowlength windowlength;
  draw_board ()
