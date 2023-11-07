import gleam/bit_string
import gleam/list
import gleam/result

pub type Error {
  IncompleteSequence
}

pub fn encode(integers: List(Int)) -> BitString {
  integers
  |> list.map(encode_one(_, True))
  |> bit_string.concat
}

fn encode_one(integer: Int, is_first_call: Bool) -> BitString {
  let leading_bit = case is_first_call {
    True -> 0
    False -> 1
  }

  let small_bits = integer % 128
  let small_byte = <<leading_bit:1, small_bits:7>>

  let larger_bytes = case integer / 128 {
    0 -> <<>>
    large_bits -> encode_one(large_bits, False)
  }
  bit_string.append(larger_bytes, small_byte)
}

pub fn decode(string: BitString) -> Result(List(Int), Error) {
  do_decode(string, 0)
}

fn do_decode(string: BitString, acc: Int) -> Result(List(Int), Error) {
  case string {
    <<>> -> Ok([])
    <<1:1, _:7>> -> Error(IncompleteSequence)
    <<1:1, large_bits:7, rest:bits>> -> do_decode(rest, 128 * acc + large_bits)
    <<0:1, last_bits:7, rest:bits>> -> {
      use integers <- result.then(do_decode(rest, 0))
      Ok([128 * acc + last_bits, ..integers])
    }
  }
}
