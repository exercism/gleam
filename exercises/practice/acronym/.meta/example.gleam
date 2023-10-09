import gleam/list
import gleam/regex
import gleam/string

pub fn abbreviate(phrase phrase: String) -> String {
  let assert Ok(re) = regex.from_string("[ +\\-/;_]+")
  regex.split(with: re, content: phrase)
  |> list.fold(
    "",
    fn(acc, val) {
      let assert Ok(letter) =
        val
        |> string.capitalise
        |> string.first
      acc <> letter
    },
  )
}
