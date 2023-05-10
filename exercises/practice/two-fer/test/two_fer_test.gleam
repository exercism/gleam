import two_fer.{two_fer}
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
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
