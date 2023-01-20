import hello_world
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn say_hi_test() {
  hello_world.hello()
  |> should.equal("Hello, World!")
}
