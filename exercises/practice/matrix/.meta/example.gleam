import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn row(index: Int, matrix: String) -> Result(List(Int), Nil) {
  matrix
  |> parse_rows()
  |> result.then(fn(rows) { list.at(rows, index - 1) })
}

pub fn column(index: Int, matrix: String) -> Result(List(Int), Nil) {
  matrix
  |> parse_rows()
  |> result.then(fn(rows) {
    rows
    |> list.transpose()
    |> list.at(index - 1)
  })
}

fn parse_rows(input: String) -> Result(List(List(Int)), Nil) {
  input
  |> string.split("\n")
  |> list.try_map(fn(rows) {
    rows
    |> string.split(" ")
    |> list.try_map(int.parse)
  })
}
