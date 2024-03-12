import gleam/list
import gleam/string

pub fn translate(phrase: String) -> String {
  phrase
  |> string.split(" ")
  |> list.map(string.to_graphemes)
  |> list.map(do_translate(_, ""))
  |> string.join(" ")
}

pub fn do_translate(letters: List(String), consonants: String) -> String {
  case letters {
    ["a", ..]
    | ["e", ..]
    | ["i", ..]
    | ["o", ..]
    | ["u", ..]
    | ["y", "t", ..]
    | ["x", "r", ..] -> string.concat(letters) <> consonants <> "ay"
    ["q", "u", ..tail] -> string.concat(tail) <> consonants <> "quay"
    ["y", ..tail] if consonants != "" ->
      "y" <> consonants <> string.concat(tail) <> "ay"
    [hd, ..tail] -> do_translate(tail, consonants <> hd)
    _ -> string.concat(letters)
  }
}
