import gleam/list
import gleam/string
import gleam/result

pub fn distance(strand1: String, strand2: String) -> Result(Int, Nil) {
  list.strict_zip(string.to_graphemes(strand1), string.to_graphemes(strand2))
  |> result.map(fn(pairs) {
    pairs
    |> list.filter(fn(pair) { pair.0 != pair.1 })
    |> list.length
  })
  |> result.nil_error
}
