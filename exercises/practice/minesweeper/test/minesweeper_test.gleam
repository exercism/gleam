import gleeunit
import gleeunit/should
import minesweeper

pub fn main() {
  gleeunit.main()
}

pub fn no_rows_test() {
  minesweeper.annotate("")
  |> should.equal("")
}

pub fn no_mines_test() {
  minesweeper.annotate(
    "___
___
___",
  )
  |> should.equal(
    "___
___
___",
  )
}

pub fn minefield_with_only_mines_test() {
  minesweeper.annotate(
    "***
***
***",
  )
  |> should.equal(
    "***
***
***",
  )
}

pub fn mine_surrounded_by_spaces_test() {
  minesweeper.annotate(
    "___
_*_
___",
  )
  |> should.equal(
    "111
1*1
111",
  )
}

pub fn space_surrounded_by_mines_test() {
  minesweeper.annotate(
    "***
*_*
***",
  )
  |> should.equal(
    "***
*8*
***",
  )
}

pub fn horizontal_line_test() {
  minesweeper.annotate("_*_*_")
  |> should.equal("1*2*1")
}

pub fn horizontal_line_mines_at_edges_test() {
  minesweeper.annotate("*___*")
  |> should.equal("*1_1*")
}

pub fn vertical_line_test() {
  minesweeper.annotate(
    "_
*
_
*
_",
  )
  |> should.equal(
    "1
*
2
*
1",
  )
}

pub fn vertical_line_mines_at_edges_test() {
  minesweeper.annotate(
    "*
_
_
_
*",
  )
  |> should.equal(
    "*
1
_
1
*",
  )
}

pub fn cross_test() {
  minesweeper.annotate(
    "__*__
__*__
*****
__*__
__*__",
  )
  |> should.equal(
    "_2*2_
25*52
*****
25*52
_2*2_",
  )
}

pub fn large_minefield_test() {
  minesweeper.annotate(
    "_*__*_
__*___
____*_
___*_*
_*__*_
______",
  )
  |> should.equal(
    "1*22*1
12*322
_123*2
112*4*
1*22*2
111111",
  )
}
