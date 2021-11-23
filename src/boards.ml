(*NOTE: ocamlformat is ignored for this file to reduce board sizes.*)

(*This file will contain all of the boards/data to use in state and tests.*)

open Game
open Helper

let starting_board : Game.t =
  [
    (*Column 1*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
    (*Column 2*)
    [ Some (White, Knight); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 3*)
    [ Some (White, Bishop); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 4*)
    [ Some (White, Queen); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Queen);];
    (*Column 5*)
    [ Some (White, King); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, King);];
    (*Column 6*)
    [ Some (White, Bishop); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 7*)
    [ Some (White, Knight); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 8*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
  ]

let starting_board_init_moves : move list = 
  [((0, 1), (0, 3)); ((0, 1), (0, 2)); ((1, 0), (2, 2)); ((1, 0), (0, 2)); 
  ((1, 1), (1, 3)); ((1, 1), (1, 2)); ((2, 1), (2, 3)); ((2, 1), (2, 2)); 
  ((3, 1), (3, 3)); ((3, 1), (3, 2)); ((4, 1), (4, 3)); ((4, 1), (4, 2)); 
  ((5, 1), (5, 3)); ((5, 1), (5, 2)); ((6, 0), (7, 2)); ((6, 0), (5, 2)); 
  ((6, 1), (6, 3)); ((6, 1), (6, 2)); ((7, 1), (7, 3)); ((7, 1), (7, 2))]

let empty_with_piece piece : Game.t = 
  [
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; piece; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
  ]

let knight_board : Game.t = 
  [
    [ None; None; Some (White, Knight); None; None; None; None; None;];
    [ None; None; None; None; Some (Black, Rook); None; None; None;];
    [ None; Some (White, Queen); None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
  ]

let king_board1 : Game.t = 
  [
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ Some (White, King); None; None; None; None; None; None; Some (White, Bishop);];
    [ None; None; None; None; None; None; Some (Black, Queen); Some (Black, King);];
    [ Some (White, Rook); None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
  ]
let king_board_enemy_moves : move list = 
  squares_to_moves (4, 0) [(4, 1); (4, 2); (4, 3); (4, 4); (4, 5); (4, 6); (4, 7); ]   

let king_board2 : Game.t = 
  [
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ Some (White, King); None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ Some (White, Rook); None; None; None; None; Some (Black, King); None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
  ]
  
let king_board3 : Game.t = 
  [
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ Some (White, King); None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; Some (White, Pawn); None; Some (Black, King); None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
  ]

let king_board4 : Game.t = 
  [
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ Some (White, King); None; None; None; None; None; None; None;];
    [ None; None; None; None; Some (White, Rook); None; None; None;];
    [ None; None; None; None; None; Some (Black, King); None; None;];
    [ None; None; Some (White, Bishop); None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
  ]

let move_checker_board1 : Game.t = 
  [
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
    [ None; Some (White, Pawn); None; Some (Black, Bishop); None; None; Some (Black, Pawn); Some (Black, Knight);];
    [ Some (White, Bishop); Some (White, Pawn); Some (White, Knight)(*not pinned*); None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    [ Some (White, Queen); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Queen);];
    [ Some (White, King); Some (White, Pawn); None; None; None; Some (Black, Pawn); None; Some (Black, King);];
    [ Some (White, Bishop); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    [ Some (White, Knight); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
  ]

let move_checker_board2 : Game.t = 
  [
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
    [ None; Some (White, Pawn); None; Some (Black, Bishop); None; None; Some (Black, Pawn); Some (Black, Knight);];
    [ Some (White, Bishop); Some (White, Pawn); Some (White, Knight)(*pinned*); None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    [ Some (White, Queen); None; None; Some (White, Pawn); None; None; Some (Black, Pawn); Some (Black, Queen);];
    [ Some (White, King); Some (White, Pawn); None; None; None; Some (Black, Pawn); None; Some (Black, King);];
    [ Some (White, Bishop); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    [ Some (White, Knight); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
  ]
  
let move_checker_board3 : Game.t = 
  [
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ Some (White, King); None; None; None; None; None; None; None;];
    [ Some (White, Rook); None; None; None; None; None; Some (Black, Queen); Some (Black, King);];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
  ]
  
let move_checker_board4 : Game.t = 
  [
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ Some (White, King); None; None; None; None; None; None; None;];
    [ Some (White, Rook); None; None; None; Some (Black, King); None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; Some (Black, Bishop); None; None; None; None; None; None;];
  ]
  
let move_checker_board5 : Game.t = 
  [
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
    [ None; Some (White, Pawn); None; Some (Black, Bishop); None; None; Some (Black, Pawn); Some (Black, Knight);];
    [ Some (White, Bishop); Some (White, Pawn); Some (White, Knight)(*pinned*); None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    [ Some (White, Queen); None; None; Some (White, Pawn); None; None; Some (Black, Pawn); Some (Black, Queen);];
    [ Some (White, King); None; None; None; None; Some (Black, Rook); None; Some (Black, King);];
    [ Some (White, Bishop); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    [ Some (White, Knight); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
  ] 
  
let move_checker_board6 : Game.t = 
  [
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ Some (White, King); None; None; None; None; None; None; None;];
    [ Some (White, Rook); None; None; None; None; None; Some (Black, King); None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; Some (Black, Bishop); None; None; None; None; None; None;];
  ]  
let bishop_board : Game.t = 
  [
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ Some (White, Bishop); None; None; None; None; None; None; None;];
    [ None; Some (Black, Bishop); None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
  ]

let bishop_empty_board_coords = [
  (0, 0); (1, 1); (2, 2); (4, 4); (5, 5); (6, 6); (7, 7); (0, 6); (1, 5);
  (2, 4); (4, 2); (5, 1); (6, 0);
]
let rook_board : Game.t = 
  [
    [ Some (White, Rook); None; Some (Black, Rook); None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
  ]

let rook_empty_board_coords = [
  (3, 0); (3, 1); (3, 2); (3, 4); (3, 5); (3, 6); (3, 7); (0, 3); (1, 3); 
  (2, 4); (4, 5); (5, 6); (6, 7);
]
let queen_board : Game.t = 
  [
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ Some (White, Queen); None; Some (Black, Queen); None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
    [ None; None; None; None; None; None; None; None;];
  ]

let rook_empty_board_coords = [
  (0, 3); (1, 3); (2, 3); (4, 3); (5, 3); (6, 3); (7, 3); (3, 0); (3, 1);
  (3, 2); (3, 4); (3, 5); (3, 6); (3, 7);
]

let rook_path_interference_coords = [
  (0, 1); (0, 2); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0); (7, 0);
]

let queen_empty_board_coords = [
  (0, 0); (1, 1); (2, 2); (4, 4); (5, 5); (6, 6); (7, 7); (6, 0); (5, 1);
  (4, 2); (2, 4); (1, 5); (0, 6); (3, 0); (3, 1); (3, 2); (3, 4); (3, 5);
  (3, 6); (3, 7); (0, 3); (1, 3); (2, 3); (4, 3); (5, 3); (6, 3); (7, 3);
]

let queen_path_interference_coords = [
  (0, 0); (1, 0); (2, 0); (4, 0); (5, 0); (6, 0); (7, 0); (2, 1); (1, 2);
  (0, 3); (4, 1); (5, 2); (6, 3); (7, 4); (3, 1); (3, 2);
]

let starting_board_pawn1 : Game.t =
  [
    (*Column 1*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
    (*Column 2*)
    [ Some (White, Knight); Some (White, Pawn); None; None; None; Some (Black, Pawn); None; Some (Black, Knight);];
    (*Column 3*)
    [ Some (White, Bishop); None; None; None; Some (White, Pawn); None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 4*)
    [ Some (White, Queen); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Queen);];
    (*Column 5*)
    [ Some (White, King); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, King);];
    (*Column 6*)
    [ Some (White, Bishop); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 7*)
    [ Some (White, Knight); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 8*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
  ]

let starting_board_pawn2 : Game.t =
  [
    (*Column 1*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
    (*Column 2*)
    [ Some (White, Knight); Some (White, Pawn); None; None; Some (Black, Pawn); None; None; Some (Black, Knight);];
    (*Column 3*)
    [ Some (White, Bishop); None; None; None; Some (White, Pawn); None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 4*)
    [ Some (White, Queen); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Queen);];
    (*Column 5*)
    [ Some (White, King); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, King);];
    (*Column 6*)
    [ Some (White, Bishop); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 7*)
    [ Some (White, Knight); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 8*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
  ]

let starting_board_update1 : Game.t =
  [
    (*Column 1*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
    (*Column 2*)
    [ Some (White, Knight); None; Some (White, Pawn); None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 3*)
    [ Some (White, Bishop); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 4*)
    [ Some (White, Queen); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Queen);];
    (*Column 5*)
    [ Some (White, King); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, King);];
    (*Column 6*)
    [ Some (White, Bishop); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 7*)
    [ Some (White, Knight); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 8*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
  ]

let starting_board_update2 : Game.t =
  [
    (*Column 1*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
    (*Column 2*)
    [ Some (White, Knight); None; None; Some (White, Pawn); None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 3*)
    [ Some (White, Bishop); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 4*)
    [ Some (White, Queen); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Queen);];
    (*Column 5*)
    [ Some (White, King); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, King);];
    (*Column 6*)
    [ Some (White, Bishop); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 7*)
    [ Some (White, Knight); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 8*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
  ]



let castle1 : Game.t =
  [
    (*Column 1*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
    (*Column 2*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 3*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 4*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Queen);];
    (*Column 5*)
    [ Some (White, King); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, King);];
    (*Column 6*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 7*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 8*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
  ](*castle works*)


let castle2 : Game.t =
  [
    (*Column 1*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
    (*Column 2*)
    [ Some (White, Knight); Some (White, Pawn);None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 3*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 4*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Queen);];
    (*Column 5*)
    [ Some (White, King); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, King);];
    (*Column 6*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 7*)
    [ Some (White, Knight); None; None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 8*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
  ](*castle blocked by piece*)



let castle3 : Game.t =
  [
    (*Column 1*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); None;];
    (*Column 2*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 3*)
    [ None; None; None; None; None; Some (Black, Rook); Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 4*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Queen);];
    (*Column 5*)
    [ Some (White, King); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, King);];
    (*Column 6*)
    [ None; None; None; None; None;Some (Black, Rook); Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 7*)
    [None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 8*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); None;];
  ](*open sqaure in check*)

let castle4 : Game.t =
  [
    (*Column 1*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
    (*Column 2*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 3*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 4*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Queen);];
    (*Column 5*)
    [ Some (White, King); None; None; None; None; Some (Black, Rook); Some (Black, Pawn); Some (Black, King);];
    (*Column 6*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 7*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Knight);];
    (*Column 8*)
    [ Some (White, Rook); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
  ](*king in check*)

let castle5 : Game.t =
  [
    (*Column 1*)
    [ Some (Black, Knight); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
    (*Column 2*)
    [None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); None;];
    (*Column 3*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 4*)
    [ None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Queen);];
    (*Column 5*)
    [ Some (White, King); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, King);];
    (*Column 6*)
    [None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Bishop);];
    (*Column 7*)
    [None; Some (White, Pawn); None; None; None; None; Some (Black, Pawn); None;];
    (*Column 8*)
    [ Some (Black, Knight); Some (White, Pawn); None; None; None; None; Some (Black, Pawn); Some (Black, Rook);];
  ](*rook captured*)