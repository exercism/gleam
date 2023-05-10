import gleam/map.{Map}
import gleam/list
import gleam/regex
import gleam/string
import gleam/option

pub fn count_words(input: String) -> Map(String, Int) {
  let assert Ok(re) = regex.from_string("[a-z0-9]+('[a-z0-9]+)?")
  input
  |> string.lowercase
  |> regex.scan(re, _)
  |> list.map(fn(m) { m.content })
  |> list.fold(map.new(), increment)
}

fn increment(counts: Map(String, Int), word: String) -> Map(String, Int) {
  map.update(counts, word, fn(previous) { option.unwrap(previous, 0) + 1 })
}
