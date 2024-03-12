import exercism/should
import exercism/test_runner
import hello_world

pub fn main() {
  test_runner.main()
}

pub fn say_hi_test() {
  hello_world.hello()
  |> should.equal("Hello, World!")
}
