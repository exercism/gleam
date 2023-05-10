import gleam/string
import gleam/map.{from_list, get}
import gleam/list.{fold}

pub fn score(word word: String) -> Int {
  let word_values =
    from_list([
      #("a", 1),
      #("b", 3),
      #("c", 3),
      #("d", 2),
      #("e", 1),
      #("f", 4),
      #("g", 2),
      #("h", 4),
      #("i", 1),
      #("j", 8),
      #("k", 5),
      #("l", 1),
      #("m", 3),
      #("n", 1),
      #("o", 1),
      #("p", 3),
      #("q", 10),
      #("r", 1),
      #("s", 1),
      #("t", 1),
      #("u", 1),
      #("v", 4),
      #("w", 4),
      #("x", 8),
      #("y", 4),
      #("z", 10),
    ])

  word
  |> string.lowercase
  |> string.to_graphemes
  |> fold(
    0,
    fn(acc, x) {
      case get(word_values, x) {
        Ok(value) -> acc + value
        _ -> acc
      }
    },
  )
}
