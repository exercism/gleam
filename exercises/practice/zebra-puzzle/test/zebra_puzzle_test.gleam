import exercism/should
import exercism/test_runner
import zebra_puzzle

pub fn main() {
  test_runner.main()
}

pub fn drinks_water_test() {
  zebra_puzzle.water_drinker()
  |> should.equal(Ok("Norwegian"))
}

pub fn owns_zebra_test() {
  zebra_puzzle.zebra_owner()
  |> should.equal(Ok("Japanese"))
}
