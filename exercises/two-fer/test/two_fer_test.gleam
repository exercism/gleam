import two_fer.{two_fer}
import gleam/result
import gleam/expect

pub fn no_name_test() {
  two_fer(result.none())
  |> should.equal(_, "One for you, one for me")
}

pub fn with_name_test() {
  two_fer(Ok("Gilberto Barros"))
  |> should.equal(_, "One for Gilberto Barros, one for me")
}
