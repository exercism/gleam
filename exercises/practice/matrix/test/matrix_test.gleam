import exercism/should
import exercism/test_runner
import matrix

pub fn main() {
  test_runner.main()
}

pub fn extract_row_from_one_number_matrix_test() {
  matrix.row(1, "1")
  |> should.equal(Ok([1]))
}

pub fn can_extract_row_test() {
  matrix.row(2, "1 2\n3 4")
  |> should.equal(Ok([3, 4]))
}

pub fn extract_row_where_numbers_have_different_widths_test() {
  matrix.row(2, "1 2\n10 20")
  |> should.equal(Ok([10, 20]))
}

pub fn can_extract_row_from_non_square_matrix_with_no_corresponding_column_test() {
  matrix.row(4, "1 2 3\n4 5 6\n7 8 9\n8 7 6")
  |> should.equal(Ok([8, 7, 6]))
}

pub fn returns_error_if_the_given_row_does_not_exist_test() {
  matrix.row(2, "1")
  |> should.equal(Error(Nil))
}

pub fn extract_column_from_one_number_matrix_test() {
  matrix.column(1, "1")
  |> should.equal(Ok([1]))
}

pub fn can_extract_column_test() {
  matrix.column(3, "1 2 3\n4 5 6\n7 8 9")
  |> should.equal(Ok([3, 6, 9]))
}

pub fn can_extract_column_from_non_square_matrix_with_no_corresponding_row_test() {
  matrix.column(4, "1 2 3 4\n5 6 7 8\n9 8 7 6")
  |> should.equal(Ok([4, 8, 6]))
}

pub fn extract_column_where_numbers_have_different_widths_test() {
  matrix.column(2, "89 1903 3\n18 3 1\n9 4 800")
  |> should.equal(Ok([1903, 3, 4]))
}

pub fn returns_error_if_the_given_column_does_not_exist_test() {
  matrix.column(2, "1")
  |> should.equal(Error(Nil))
}
