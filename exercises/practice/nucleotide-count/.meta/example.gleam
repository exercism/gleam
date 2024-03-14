import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string

pub fn nucleotide_count(dna: String) -> Result(Dict(String, Int), Nil) {
  dna
  |> string.to_graphemes()
  |> list.try_fold(
    from: dict.from_list([#("A", 0), #("C", 0), #("G", 0), #("T", 0)]),
    with: fn(counts, letter) {
      dict.get(counts, letter)
      |> result.map(fn(count) { dict.insert(counts, letter, count + 1) })
    },
  )
}
