import gleam/string
import gleam/float
import gleam/list
import gleam/set
import gleam/int

pub fn ciphertext(plaintext: String) -> String {
  plaintext
  |> characters_to_encode()
  |> rows()
  |> list.transpose()
  |> list.map(string.concat)
  |> string.join(" ")
}

fn rows(characters: List(String)) -> List(List(String)) {
  let width = square_width(characters)

  characters
  |> list.sized_chunk(into: width)
  |> list.map(fn(row) {
    row
    |> string.concat()
    |> string.pad_right(width, " ")
    |> string.to_graphemes()
  })
}

fn characters_to_encode(plaintext: String) -> List(String) {
  let valid_characters =
    "abcdefghijklmnopqrstuvwxyz01234567890"
    |> string.to_graphemes()
    |> set.from_list()

  plaintext
  |> string.lowercase()
  |> string.to_graphemes()
  |> list.filter(set.contains(valid_characters, _))
}

fn square_width(characters: List(String)) -> Int {
  assert Ok(sqrt) =
    characters
    |> list.length()
    |> int.square_root()

  sqrt
  |> float.ceiling()
  |> float.round()
}
