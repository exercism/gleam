import gleam/map.{Map}
import gleam/string
import gleam/list
import gleam/option.{None, Some}

pub fn nucleotide_count(dna: String) -> Result(Map(String, Int), Nil) {
  dna
  |> string.to_graphemes()
  |> list.try_fold(
    map.from_list([#("A", 0), #("C", 0), #("G", 0), #("T", 0)]),
    fn(counts, letter) {
      case map.has_key(counts, letter) {
        True ->
          Ok(map.update(
            counts,
            letter,
            fn(count) { option.unwrap(count, 0) + 1 },
          ))
        False -> Error(Nil)
      }
    },
  )
}
