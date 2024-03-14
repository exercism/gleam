import exercism/should
import exercism/test_runner
import gleam/option.{None, Some}
import two_fer.{two_fer}

pub fn main() {
  test_runner.main()
}

pub fn no_name_given_test() {
  two_fer(None)
  |> should.equal("One for you, one for me.")
}

pub fn a_name_given_test() {
  two_fer(Some("Alice"))
  |> should.equal("One for Alice, one for me.")
}

pub fn another_name_given_test() {
  two_fer(Some("Bob"))
  |> should.equal("One for Bob, one for me.")
}
