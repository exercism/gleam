import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn largest_product(digits: String, span: Int) -> Result(Int, Nil) {
  let characters = string.to_graphemes(digits)
  let num_characters = list.length(characters)

  case span {
    0 -> Ok(1)
    _ if span < 0 || span > num_characters -> Error(Nil)
    _ ->
      characters
      |> list.try_map(int.parse)
      |> result.map(list.window(_, by: span))
      |> result.map(fn(slices) {
        slices
        |> list.map(int.product)
        |> list.sort(fn(a, b) { int.compare(b, a) })
        |> list.first
        |> result.unwrap(1)
      })
  }
}
