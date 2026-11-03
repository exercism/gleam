import exercism/should
import exercism/test_runner
import flower_field

pub fn main() {
  test_runner.main()
}

pub fn no_rows_test() {
  flower_field.annotate("")
  |> should.equal("")
}

pub fn no_flowers_test() {
  flower_field.annotate(
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

pub fn garden_full_of_flowers_test() {
  flower_field.annotate(
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

pub fn flower_surrounded_by_spaces_test() {
  flower_field.annotate(
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

pub fn space_surrounded_by_flowers_test() {
  flower_field.annotate(
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
  flower_field.annotate("_*_*_")
  |> should.equal("1*2*1")
}

pub fn horizontal_line_flowers_at_edges_test() {
  flower_field.annotate("*___*")
  |> should.equal("*1_1*")
}

pub fn vertical_line_test() {
  flower_field.annotate(
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

pub fn vertical_line_flowers_at_edges_test() {
  flower_field.annotate(
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
  flower_field.annotate(
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

pub fn large_garden_test() {
  flower_field.annotate(
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
