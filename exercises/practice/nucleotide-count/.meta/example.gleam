import gleam/map.{Map}
import gleam/string
import gleam/list
import gleam/result

pub fn nucleotide_count(dna: String) -> Result(Map(String, Int), Nil) {
  dna
  |> string.to_graphemes()
  |> list.try_fold(
    from: map.from_list([#("A", 0), #("C", 0), #("G", 0), #("T", 0)]),
    with: fn(counts, letter) {
      map.get(counts, letter)
      |> result.map(fn(count) { map.insert(counts, letter, count + 1) })
    },
  )
}
