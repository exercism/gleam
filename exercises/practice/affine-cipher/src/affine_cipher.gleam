import gleam/string
import gleam/result
import gleam/list
import gleam/int

pub fn encode(
  plaintext plaintext: String,
  a a: Int,
  b b: Int,
) -> Result(String, Nil) {
  case modular_inverse(a, alphabet_length) {
    Ok(_) ->
      plaintext
      |> translate(fn(index) { a * index + b })
      |> string.to_graphemes()
      |> list.sized_chunk(into: 5)
      |> list.map(string.concat)
      |> string.join(with: " ")
      |> Ok
    Error(_) -> Error(Nil)
  }
}

pub fn decode(
  ciphertext ciphertext: String,
  a a: Int,
  b b: Int,
) -> Result(String, Nil) {
  result.map(
    modular_inverse(a, alphabet_length),
    fn(mmi) { translate(ciphertext, fn(index) { mmi * { index - b } }) },
  )
}

fn translate(input: String, op: fn(Int) -> Int) -> String {
  let letters = string.to_utf_codepoints("abcdefghijklmnopqrstuvwxyz")
  let digits = string.to_utf_codepoints("0123456789")

  input
  |> string.lowercase()
  |> string.to_utf_codepoints()
  |> list.filter_map(fn(char) {
    case list.contains(letters, char) {
      True -> {
        assert Ok(shifted_index) =
          int.modulo(
            op(string.utf_codepoint_to_int(char) - letter_a_int_code_point),
            alphabet_length,
          )

        string.utf_codepoint(letter_a_int_code_point + shifted_index)
      }
      False ->
        case list.contains(digits, char) {
          True -> Ok(char)
          False -> Error(Nil)
        }
    }
  })
  |> string.from_utf_codepoints()
}

fn modular_inverse(a: Int, b: Int) -> Result(Int, Nil) {
  case gcd_ext(a, b) {
    #(s, _, 1) -> Ok(s)
    _ -> Error(Nil)
  }
}

fn gcd_ext(a: Int, b: Int) -> #(Int, Int, Int) {
  case b {
    0 -> #(1, 0, a)
    _ -> {
      let q = a / b
      let r = a % b
      let #(s, t, g) = gcd_ext(b, r)
      #(t, s - q * t, g)
    }
  }
}

const alphabet_length = 26

const letter_a_int_code_point = 97
