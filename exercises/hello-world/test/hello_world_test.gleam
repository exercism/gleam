import hello_world
import gleam/should

pub fn hello_world_test() {
  hello_world.hello()
  |> should.equal(_, "Hello, world!")
}
