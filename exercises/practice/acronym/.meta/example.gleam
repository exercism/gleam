import gleam/list
import gleam/regexp
import gleam/string

pub fn abbreviate(phrase phrase: String) -> String {
  let assert Ok(re) = regexp.from_string("[ +\\-/;_]+")
  regexp.split(with: re, content: phrase)
  |> list.fold("", fn(acc, val) {
    let assert Ok(letter) =
      val
      |> string.capitalise
      |> string.first
    acc <> letter
  })
}
