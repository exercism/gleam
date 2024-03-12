import exercism/should
import exercism/test_runner
import run_length_encoding

pub fn main() {
  test_runner.main()
}

pub fn encode_empty_string_test() {
  run_length_encoding.encode("")
  |> should.equal("")
}

pub fn encode_single_characters_only_are_encoded_without_count_test() {
  run_length_encoding.encode("XYZ")
  |> should.equal("XYZ")
}

pub fn encode_string_with_no_single_characters_test() {
  run_length_encoding.encode("AABBBCCCC")
  |> should.equal("2A3B4C")
}

pub fn encode_single_characters_mixed_with_repeated_characters_test() {
  run_length_encoding.encode(
    "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB",
  )
  |> should.equal("12WB12W3B24WB")
}

pub fn encode_multiple_whitespace_mixed_in_string_test() {
  run_length_encoding.encode("  hsqq qww  ")
  |> should.equal("2 hs2q q2w2 ")
}

pub fn encode_lowercase_characters_test() {
  run_length_encoding.encode("aabbbcccc")
  |> should.equal("2a3b4c")
}

pub fn decode_empty_string_test() {
  run_length_encoding.decode("")
  |> should.equal("")
}

pub fn decode_single_characters_only_test() {
  run_length_encoding.decode("XYZ")
  |> should.equal("XYZ")
}

pub fn decode_string_with_no_single_characters_test() {
  run_length_encoding.decode("2A3B4C")
  |> should.equal("AABBBCCCC")
}

pub fn decode_single_characters_with_repeated_characters_test() {
  run_length_encoding.decode("12WB12W3B24WB")
  |> should.equal("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB")
}

pub fn decode_multiple_whitespace_mixed_in_string_test() {
  run_length_encoding.decode("2 hs2q q2w2 ")
  |> should.equal("  hsqq qww  ")
}

pub fn decode_lowercase_string_test() {
  run_length_encoding.decode("2a3b4c")
  |> should.equal("aabbbcccc")
}

pub fn encode_followed_by_decode_gives_original_string_test() {
  run_length_encoding.encode("zzz ZZ  zZ")
  |> run_length_encoding.decode()
  |> should.equal("zzz ZZ  zZ")
}
