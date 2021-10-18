(*NOTE: ocamlformat is ignored for this file to reduce board sizes.*)

(*This file will contain all of the boards to use in state and tests.*)

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

let king_board : Game.t = 
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