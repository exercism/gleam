import exercism/should
import exercism/test_runner
import rectangles

pub fn main() {
  test_runner.main()
}

pub fn no_rows_test() {
  rectangles.rectangles("")
  |> should.equal(0)
}

pub fn no_columns_test() {
  rectangles.rectangles(
    "
     
    ",
  )
  |> should.equal(0)
}

pub fn no_rectangles_test() {
  rectangles.rectangles(" ")
  |> should.equal(0)
}

pub fn one_rectangle_test() {
  rectangles.rectangles(
    "
    +-+
    | |
    +-+
    ",
  )
  |> should.equal(1)
}

pub fn two_rectangles_without_shared_parts_test() {
  rectangles.rectangles(
    "
      +-+
      | |
    +-+-+
    | |  
    +-+  
    ",
  )
  |> should.equal(2)
}

pub fn five_rectangles_with_shared_parts_test() {
  rectangles.rectangles(
    "
      +-+
      | |
    +-+-+
    | | |
    +-+-+
    ",
  )
  |> should.equal(5)
}

pub fn rectangle_of_height_1_is_counted_test() {
  rectangles.rectangles(
    "
    +--+
    +--+
    ",
  )
  |> should.equal(1)
}

pub fn rectangle_of_width_1_is_counted_test() {
  rectangles.rectangles(
    "
    ++
    ||
    ++
    ",
  )
  |> should.equal(1)
}

pub fn one_by_one_square_is_counted_test() {
  rectangles.rectangles(
    "
    ++
    ++
    ",
  )
  |> should.equal(1)
}

pub fn only_complete_rectangles_are_counted_test() {
  rectangles.rectangles(
    "
      +-+
        |
    +-+-+
    | | -
    +-+-+
    ",
  )
  |> should.equal(1)
}

pub fn rectangles_can_be_of_different_sizes_test() {
  rectangles.rectangles(
    "
      +------+----+
      |      |    |
      +---+--+    |
      |   |       |
      +---+-------+
    ",
  )
  |> should.equal(3)
}

pub fn corner_is_required_for_a_rectangle_to_be_complete_test() {
  rectangles.rectangles(
    "
      +------+----+
      |      |    |
      +------+    |
      |   |       |
      +---+-------+
    ",
  )
  |> should.equal(2)
}

pub fn large_input_with_many_rectangles_test() {
  rectangles.rectangles(
    "
      +---+--+----+
      |   +--+----+
      +---+--+    |
      |   +--+----+
      +---+--+--+-+
      +---+--+--+-+
      +------+  | |
                +-+
    ",
  )
  |> should.equal(60)
}

pub fn rectangles_must_have_four_sides_test() {
  rectangles.rectangles(
    "
      +-+ +-+
      | | | |
      +-+-+-+
        | |  
      +-+-+-+
      | | | |
      +-+ +-+
    ",
  )
  |> should.equal(5)
}
