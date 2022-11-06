import pascals_triangle
import gleam/list
import gleam/result
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn one_row_test() {
  pascals_triangle.rows(1)
  |> should.equal([[1]])
}

pub fn two_rows_test() {
  pascals_triangle.rows(2)
  |> should.equal([[1], [1, 1]])
}

pub fn three_rows_test() {
  pascals_triangle.rows(3)
  |> should.equal([[1], [1, 1], [1, 2, 1]])
}

pub fn fourth_row_test() {
  pascals_triangle.rows(4)
  |> list.last
  |> result.unwrap([])
  |> should.equal([1, 3, 3, 1])
}

pub fn fifth_row_test() {
  pascals_triangle.rows(5)
  |> list.last
  |> result.unwrap([])
  |> should.equal([1, 4, 6, 4, 1])
}

pub fn twentieth_row_test() {
  pascals_triangle.rows(20)
  |> list.last
  |> result.unwrap([])
  |> should.equal([
    1, 19, 171, 969, 3876, 11_628, 27_132, 50_388, 75_582, 92_378, 92_378,
    75_582, 50_388, 27_132, 11_628, 3876, 969, 171, 19, 1,
  ])
}
