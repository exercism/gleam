import gleam/list.{fold}
import gleam/regex.{from_string, split}
import gleam/string.{capitalise, first}

pub fn abbreviate(phrase phrase: String) -> String {
  assert Ok(re) = from_string("[ +\\-/;_]+")
  split(with: re, content: phrase)
  |> fold(
    "",
    fn(acc, val) {
      assert Ok(letter) =
        val
        |> capitalise
        |> first
      acc <> letter
    },
  )
}
