import gleam/iterator
import gleam/string
import gleam/regex
import gleeunit
import gleeunit/should
import simple_cipher

pub fn main() {
  gleeunit.main()
}

pub fn substitution_cipher_can_encode_test() {
  simple_cipher.encode(plaintext: "aaaaaaaaaa", key: "abcdefghij")
  |> should.equal("abcdefghij")
}

pub fn substitution_cipher_can_decode_test() {
  simple_cipher.decode(ciphertext: "abcdefghij", key: "abcdefghij")
  |> should.equal("aaaaaaaaaa")
}

pub fn substitution_cipher_is_reversible_i_e_if_you_apply_decode_in_a_encoded_result_you_must_see_the_same_plaintext_encode_parameter_as_a_result_of_the_decode_method_test() {
  simple_cipher.encode(plaintext: "abcdefghij", key: "abcdefghij")
  |> simple_cipher.decode(key: "abcdefghij")
  |> should.equal("abcdefghij")
}

pub fn substitution_cipher_can_double_shift_encode_test() {
  simple_cipher.encode(plaintext: "iamapandabear", key: "iamapandabear")
  |> should.equal("qayaeaagaciai")
}

pub fn substitution_cipher_can_wrap_on_encode_test() {
  simple_cipher.encode(plaintext: "zzzzzzzzzz", key: "abcdefghij")
  |> should.equal("zabcdefghi")
}

pub fn substitution_cipher_can_wrap_on_decode_test() {
  simple_cipher.decode(ciphertext: "zabcdefghi", key: "abcdefghij")
  |> should.equal("zzzzzzzzzz")
}

pub fn substitution_cipher_can_encode_messages_longer_than_the_key_test() {
  simple_cipher.encode(plaintext: "iamapandabear", key: "abc")
  |> should.equal("iboaqcnecbfcr")
}

pub fn substitution_cipher_can_decode_messages_longer_than_the_key_test() {
  simple_cipher.decode(ciphertext: "iboaqcnecbfcr", key: "abc")
  |> should.equal("iamapandabear")
}

pub fn random_key_cipher_key_is_made_only_of_lowercase_letters_test() {
  assert Ok(re) = regex.from_string("^[a-z]+$")

  iterator.repeatedly(simple_cipher.generate_key)
  |> iterator.take(100)
  |> iterator.all(fn(key) { assert True = regex.check(re, key) })
}

pub fn random_key_cipher_can_encode_test() {
  let plaintext = "aaaaaaaaaa"
  let key = simple_cipher.generate_key()

  simple_cipher.encode(plaintext: plaintext, key: key)
  |> should.equal(string.slice(key, 0, string.length(plaintext)))
}

pub fn random_key_cipher_can_decode_test() {
  let key = simple_cipher.generate_key()
  let expected = "aaaaaaaaaa"

  simple_cipher.decode(
    ciphertext: string.slice(key, 0, string.length(expected)),
    key: key,
  )
  |> should.equal(expected)
}

pub fn random_key_cipher_is_reversible_i_e_if_you_apply_decode_in_a_encoded_result_you_must_see_the_same_plaintext_encode_parameter_as_a_result_of_the_decode_method_test() {
  let key = simple_cipher.generate_key()

  simple_cipher.encode(plaintext: "abcdefghij", key: key)
  |> simple_cipher.decode(key: key)
  |> should.equal("abcdefghij")
}
