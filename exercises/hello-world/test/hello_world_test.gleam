import hello_world
import gleam/expect

pub fn hello_world_test() {
  hello_world.hello()
  |> expect.equal(_, "Hello, world!")
}
