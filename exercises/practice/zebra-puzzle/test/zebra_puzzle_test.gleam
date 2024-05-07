import exercism/should
import exercism/test_runner
import zebra_puzzle

pub fn main() {
  test_runner.main()
}

pub fn puzzle_test() {
  let assert Ok(solution) = zebra_puzzle.solve()

  solution.water_drinker
  |> should.equal("Norwegian")

  solution.zebra_owner
  |> should.equal("Japanese")
}
