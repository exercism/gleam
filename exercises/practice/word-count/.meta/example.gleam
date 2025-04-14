import gleam/dict.{type Dict}
import gleam/list
import gleam/option
import gleam/regexp
import gleam/string

pub fn count_words(input: String) -> Dict(String, Int) {
  let assert Ok(re) = regexp.from_string("[a-z0-9]+('[a-z0-9]+)?")
  input
  |> string.lowercase
  |> regexp.scan(re, _)
  |> list.map(fn(m) { m.content })
  |> list.fold(dict.new(), increment)
}

fn increment(counts: Dict(String, Int), word: String) -> Dict(String, Int) {
  dict.upsert(counts, word, fn(previous) { option.unwrap(previous, 0) + 1 })
}
