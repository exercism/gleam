import exercism/should
import exercism/test_runner
import secret_handshake.{CloseYourEyes, DoubleBlink, Jump, Wink}

pub fn main() {
  test_runner.main()
}

pub fn wink_for_1_test() {
  secret_handshake.commands(1)
  |> should.equal([Wink])
}

pub fn double_blink_for_10_test() {
  secret_handshake.commands(2)
  |> should.equal([DoubleBlink])
}

pub fn close_your_eyes_for_100_test() {
  secret_handshake.commands(4)
  |> should.equal([CloseYourEyes])
}

pub fn jump_for_1000_test() {
  secret_handshake.commands(8)
  |> should.equal([Jump])
}

pub fn combine_two_actions_test() {
  secret_handshake.commands(3)
  |> should.equal([Wink, DoubleBlink])
}

pub fn reverse_two_actions_test() {
  secret_handshake.commands(19)
  |> should.equal([DoubleBlink, Wink])
}

pub fn reversing_one_action_gives_the_same_action_test() {
  secret_handshake.commands(24)
  |> should.equal([Jump])
}

pub fn reversing_no_actions_still_gives_no_actions_test() {
  secret_handshake.commands(16)
  |> should.equal([])
}

pub fn all_possible_actions_test() {
  secret_handshake.commands(15)
  |> should.equal([Wink, DoubleBlink, CloseYourEyes, Jump])
}

pub fn reverse_all_possible_actions_test() {
  secret_handshake.commands(31)
  |> should.equal([Jump, CloseYourEyes, DoubleBlink, Wink])
}

pub fn do_nothing_for_zero_test() {
  secret_handshake.commands(0)
  |> should.equal([])
}
