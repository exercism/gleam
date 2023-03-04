import gleeunit
import gleeunit/should
import connect.{O, X}

pub fn main() {
  gleeunit.main()
}

pub fn an_empty_board_has_no_winner_test() {
  connect.winner(
    "
. . . . .
 . . . . .
  . . . . .
   . . . . .
    . . . . .",
  )
  |> should.equal(Error(Nil))
}

pub fn x_can_win_on_a_1x1_board_test() {
  connect.winner("X")
  |> should.equal(Ok(X))
}

pub fn o_can_win_on_a_1x1_board_test() {
  connect.winner("O")
  |> should.equal(Ok(O))
}

pub fn only_edges_does_not_make_a_winner_test() {
  connect.winner(
    "
O O O X
 X . . X
  X . . X
   X O O O",
  )
  |> should.equal(Error(Nil))
}

pub fn illegal_diagonal_does_not_make_a_winner_test() {
  connect.winner(
    "
X O . .
 O X X X
  O X O .
   . O X .
    X X O O",
  )
  |> should.equal(Error(Nil))
}

pub fn nobody_wins_crossing_adjacent_angles_test() {
  connect.winner(
    "
X . . .
 . X O .
  O . X O
   . O . X
    . . O .",
  )
  |> should.equal(Error(Nil))
}

pub fn x_wins_crossing_from_left_to_right_test() {
  connect.winner(
    "
. O . .
 O X X X
  O X O .
   X X O X
    . O X .",
  )
  |> should.equal(Ok(X))
}

pub fn o_wins_crossing_from_top_to_bottom_test() {
  connect.winner(
    "
. O . .
 O X X X
  O O O .
   X X O X
    . O X .",
  )
  |> should.equal(Ok(O))
}

pub fn x_wins_using_a_convoluted_path_test() {
  connect.winner(
    "
. X X . .
 X . X . X
  . X . X .
   . X X . .
    O O O O O",
  )
  |> should.equal(Ok(X))
}

pub fn x_wins_using_a_spiral_path_test() {
  connect.winner(
    "
O X X X X X X X X
 O X O O O O O O O
  O X O X X X X X O
   O X O X O O O X O
    O X O X X X O X O
     O X O O O X O X O
      O X X X X X O X O
       O O O O O O O X O
        X X X X X X X X O",
  )
  |> should.equal(Ok(X))
}
