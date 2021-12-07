import hello_world
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn hello_world_test() {
  hello_world.hello()
  |> should.equal("Hello, world!")
}
