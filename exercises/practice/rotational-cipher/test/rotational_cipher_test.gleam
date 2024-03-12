import exercism/should
import exercism/test_runner
import rotational_cipher

pub fn main() {
  test_runner.main()
}

pub fn rotate_a_by_0_same_output_as_input_test() {
  rotational_cipher.rotate(0, "a")
  |> should.equal("a")
}

pub fn rotate_a_by_1_test() {
  rotational_cipher.rotate(1, "a")
  |> should.equal("b")
}

pub fn rotate_a_by_26_same_output_as_input_test() {
  rotational_cipher.rotate(26, "a")
  |> should.equal("a")
}

pub fn rotate_m_by_13_test() {
  rotational_cipher.rotate(13, "m")
  |> should.equal("z")
}

pub fn rotate_n_by_13_with_wrap_around_alphabet_test() {
  rotational_cipher.rotate(13, "n")
  |> should.equal("a")
}

pub fn rotate_capital_letters_test() {
  rotational_cipher.rotate(5, "OMG")
  |> should.equal("TRL")
}

pub fn rotate_spaces_test() {
  rotational_cipher.rotate(5, "O M G")
  |> should.equal("T R L")
}

pub fn rotate_numbers_test() {
  rotational_cipher.rotate(4, "Testing 1 2 3 testing")
  |> should.equal("Xiwxmrk 1 2 3 xiwxmrk")
}

pub fn rotate_punctuation_test() {
  rotational_cipher.rotate(21, "Let's eat, Grandma!")
  |> should.equal("Gzo'n zvo, Bmviyhv!")
}

pub fn rotate_all_letters_test() {
  rotational_cipher.rotate(13, "The quick brown fox jumps over the lazy dog.")
  |> should.equal("Gur dhvpx oebja sbk whzcf bire gur ynml qbt.")
}
