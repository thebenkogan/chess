(*This file will contain all of the boards to use in state and tests.*)

open Game

let starting_board : piece option list list =
  [
    (*Column 1*)
    [
      Some (White, Rook);
      Some (White, Pawn);
      None;
      None;
      None;
      None;
      Some (Black, Pawn);
      Some (Black, Rook);
    ];
    (*Column 2*)
    [
      Some (White, Knight);
      Some (White, Pawn);
      None;
      None;
      None;
      None;
      Some (Black, Pawn);
      Some (Black, Knight);
    ];
    (*Column 3*)
    [
      Some (White, Bishop);
      Some (White, Pawn);
      None;
      None;
      None;
      None;
      Some (Black, Pawn);
      Some (Black, Bishop);
    ];
    (*Column 4*)
    [
      Some (White, Queen);
      Some (White, Pawn);
      None;
      None;
      None;
      None;
      Some (Black, Pawn);
      Some (Black, Queen);
    ];
    (*Column 5*)
    [
      Some (White, King);
      Some (White, Pawn);
      None;
      None;
      None;
      None;
      Some (Black, Pawn);
      Some (Black, King);
    ];
    (*Column 6*)
    [
      Some (White, Bishop);
      Some (White, Pawn);
      None;
      None;
      None;
      None;
      Some (Black, Pawn);
      Some (Black, Bishop);
    ];
    (*Column 7*)
    [
      Some (White, Knight);
      Some (White, Pawn);
      None;
      None;
      None;
      None;
      Some (Black, Pawn);
      Some (Black, Knight);
    ];
    (*Column 8*)
    [
      Some (White, Rook);
      Some (White, Pawn);
      None;
      None;
      None;
      None;
      Some (Black, Pawn);
      Some (Black, Rook);
    ];
  ]
