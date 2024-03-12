import crypto_square
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn empty_plaintext_results_in_an_empty_ciphertext_test() {
  crypto_square.ciphertext("")
  |> should.equal("")
}

pub fn normalization_results_in_empty_plaintext_test() {
  crypto_square.ciphertext("... --- ...")
  |> should.equal("")
}

pub fn lowercase_test() {
  crypto_square.ciphertext("A")
  |> should.equal("a")
}

pub fn remove_spaces_test() {
  crypto_square.ciphertext("  b ")
  |> should.equal("b")
}

pub fn remove_punctuation_test() {
  crypto_square.ciphertext("@1,%!")
  |> should.equal("1")
}

pub fn plaintext_with_9_characters_results_in_3_chunks_of_3_characters_test() {
  crypto_square.ciphertext("This is fun!")
  |> should.equal("tsf hiu isn")
}

pub fn plaintext_with_8_characters_results_in_3_chunks_the_last_one_with_a_trailing_space_test() {
  crypto_square.ciphertext("Chill out.")
  |> should.equal("clu hlt io ")
}

pub fn plaintext_with_54_characters_results_in_7_chunks_the_last_two_with_trailing_spaces_test() {
  crypto_square.ciphertext(
    "If man was meant to stay on the ground, god would have given us roots.",
  )
  |> should.equal(
    "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau ",
  )
}
