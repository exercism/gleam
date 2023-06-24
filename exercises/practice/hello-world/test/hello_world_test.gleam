import hello_world
import exercism/test_runner
import exercism/should

pub fn main() {
  test_runner.main()
}

pub fn say_hi_test() {
  hello_world.hello()
  |> should.equal("Hello, World!")
}
