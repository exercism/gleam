import exercism/should
import exercism/test_runner
import guessing_game

pub fn main() {
  test_runner.main()
}

pub fn reply_42_test() {
  guessing_game.reply(42)
  |> should.equal("Correct")
}

pub fn reply_41_test() {
  guessing_game.reply(41)
  |> should.equal("So close")
}

pub fn reply_43_test() {
  guessing_game.reply(43)
  |> should.equal("So close")
}

pub fn reply_40_test() {
  guessing_game.reply(40)
  |> should.equal("Too low")
}

pub fn reply_1_test() {
  guessing_game.reply(1)
  |> should.equal("Too low")
}

pub fn reply_44_test() {
  guessing_game.reply(44)
  |> should.equal("Too high")
}

pub fn reply_100_test() {
  guessing_game.reply(100)
  |> should.equal("Too high")
}
