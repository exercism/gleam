import exercism/should
import exercism/test_runner
import saddle_points.{Position}

pub fn main() {
  test_runner.main()
}

pub fn can_identify_single_saddle_point_test() {
  saddle_points.saddle_points([[9, 8, 7], [5, 3, 2], [6, 6, 7]])
  |> should.equal([Position(row: 2, column: 1)])
}

pub fn can_identify_that_empty_matrix_has_no_saddle_points_test() {
  saddle_points.saddle_points([[]])
  |> should.equal([])
}

pub fn can_identify_lack_of_saddle_points_when_there_are_none_test() {
  saddle_points.saddle_points([[1, 2, 3], [3, 1, 2], [2, 3, 1]])
  |> should.equal([])
}

pub fn can_identify_multiple_saddle_points_in_a_column_test() {
  saddle_points.saddle_points([[4, 5, 4], [3, 5, 5], [1, 5, 4]])
  |> should.equal([
    Position(row: 1, column: 2),
    Position(row: 2, column: 2),
    Position(row: 3, column: 2),
  ])
}

pub fn can_identify_multiple_saddle_points_in_a_row_test() {
  saddle_points.saddle_points([[6, 7, 8], [5, 5, 5], [7, 5, 6]])
  |> should.equal([
    Position(row: 2, column: 1),
    Position(row: 2, column: 2),
    Position(row: 2, column: 3),
  ])
}

pub fn can_identify_saddle_point_in_bottom_right_corner_test() {
  saddle_points.saddle_points([[8, 7, 9], [6, 7, 6], [3, 2, 5]])
  |> should.equal([Position(row: 3, column: 3)])
}

pub fn can_identify_saddle_points_in_a_non_square_matrix_test() {
  saddle_points.saddle_points([[3, 1, 3], [3, 2, 4]])
  |> should.equal([Position(row: 1, column: 1), Position(row: 1, column: 3)])
}

pub fn can_identify_that_saddle_points_in_a_single_column_matrix_are_those_with_the_minimum_value_test() {
  saddle_points.saddle_points([[2], [1], [4], [1]])
  |> should.equal([Position(row: 2, column: 1), Position(row: 4, column: 1)])
}

pub fn can_identify_that_saddle_points_in_a_single_row_matrix_are_those_with_the_maximum_value_test() {
  saddle_points.saddle_points([[2, 5, 3, 5]])
  |> should.equal([Position(row: 1, column: 2), Position(row: 1, column: 4)])
}
