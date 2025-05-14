import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/regexp
import gleam/result
import gleam/string

pub fn encode(plaintext: String) -> String {
  let assert Ok(re) = regexp.from_string("(.)\\1*")

  plaintext
  |> regexp.scan(re, _)
  |> list.map(fn(m) {
    let assert Ok(letter) = string.first(m.content)
    case string.length(m.content) {
      1 -> letter
      count -> int.to_string(count) <> letter
    }
  })
  |> string.concat
}

pub fn decode(ciphertext: String) -> String {
  let assert Ok(re) = regexp.from_string("(\\d*)(.)")

  ciphertext
  |> regexp.scan(re, _)
  |> list.map(fn(m) {
    case m.submatches {
      [Some(count), Some(letter)] ->
        count
        |> int.parse()
        |> result.unwrap(1)
        |> list.repeat(letter, _)
        |> string.concat
      [None, Some(letter)] -> letter
      _ -> panic as "unreachable"
    }
  })
  |> string.concat
}
