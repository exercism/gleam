// pub fn rows(n: Int) -> List(List(Int)) {
//   todo("Implement this function")
// }

import gleam/list
import gleam/pair
import gleam/result

pub fn rows(n: Int) -> List(List(Int)) {
  add_rows(n, [])
  |> list.reverse()
}

fn add_rows(n: Int, acc: List(List(Int))) -> List(List(Int)) {
  case n {
    0 -> acc
    _ ->
      list.first(acc)
      |> result.unwrap([])
      |> next_row()
      |> list.prepend(acc, _)
      |> add_rows(n - 1, _)
  }
}

fn next_row(row: List(Int)) -> List(Int) {
  row
  |> list.map_fold(from: 1, with: fn(memo, item) { #(item, item + memo) })
  |> pair.second()
  |> list.prepend(1)
}
