import gleam/int
import gleam/string
import gleam/list
import gleam/regex
import gleam/result
import gleam/option.{None, Some}

pub fn encode(plaintext: String) -> String {
  assert Ok(re) = regex.from_string("(.)\\1*")

  plaintext
  |> regex.scan(re, _)
  |> list.map(fn(m) {
    assert Ok(letter) = string.first(m.content)
    case string.length(m.content) {
      1 -> letter
      count -> int.to_string(count) <> letter
    }
  })
  |> string.concat
}

pub fn decode(ciphertext: String) -> String {
  assert Ok(re) = regex.from_string("(\\d*)(.)")

  ciphertext
  |> regex.scan(re, _)
  |> list.map(fn(m) {
    case m.submatches {
      [Some(count), Some(letter)] ->
        count
        |> int.parse()
        |> result.unwrap(1)
        |> list.repeat(letter, _)
        |> string.concat
      [None, Some(letter)] -> letter
    }
  })
  |> string.concat
}
