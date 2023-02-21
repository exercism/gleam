import gleam/int
import gleam/string
import gleam/result
import gleam/iterator

pub fn encode(plaintext plaintext: String, key key: String) -> String {
  shift(input: plaintext, key: key, by: int.add)
}

pub fn decode(ciphertext ciphertext: String, key key: String) -> String {
  shift(input: ciphertext, key: key, by: int.subtract)
}

pub fn generate_key() -> String {
  iterator.repeatedly(fn() { int.random(0, alphabet_length) })
  |> iterator.map(fn(shift) { string.utf_codepoint(min_int_code_point + shift) })
  |> iterator.take(up_to: 100)
  |> iterator.to_list()
  |> result.values()
  |> string.from_utf_codepoints()
}

fn shift(
  input input: String,
  key key: String,
  by op: fn(Int, Int) -> Int,
) -> String {
  input
  |> string.to_utf_codepoints()
  |> iterator.from_list()
  |> iterator.zip(
    key
    |> string.to_utf_codepoints()
    |> iterator.from_list()
    |> iterator.cycle(),
  )
  |> iterator.map(fn(pair) { shift_letter(pair.0, pair.1, op) })
  |> iterator.to_list()
  |> string.from_utf_codepoints()
}

fn shift_letter(
  input input: UtfCodepoint,
  key key: UtfCodepoint,
  by op: fn(Int, Int) -> Int,
) -> UtfCodepoint {
  let shifted_codepoint_int =
    op(
      string.utf_codepoint_to_int(input) - min_int_code_point,
      string.utf_codepoint_to_int(key) - min_int_code_point,
    )
  assert Ok(codepoint_int_shift) =
    int.modulo(shifted_codepoint_int, alphabet_length)
  assert Ok(shifted_codepoint) =
    string.utf_codepoint(codepoint_int_shift + min_int_code_point)

  shifted_codepoint
}

const alphabet_length = 26

const min_int_code_point = 97
