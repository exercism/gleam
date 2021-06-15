import two_fer.{two_fer}
import gleam/option.{None, Some}
import gleam/should

pub fn no_name_test() {
  two_fer(None)
  |> should.equal("One for you, one for me")
}

pub fn with_name_test() {
  two_fer(Some("Gilberto Barros"))
  |> should.equal("One for Gilberto Barros, one for me")
}
