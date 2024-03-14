import exercism/should
import exercism/test_runner
import treasure_chest.{TreasureChest, Unlocked, WrongPassword}

pub fn main() {
  test_runner.main()
}

pub fn treasure_can_be_a_string_test() {
  TreasureChest("password", "treasure")
}

pub fn treasure_can_be_an_int_test() {
  TreasureChest("password", 5)
}

pub fn treasure_is_returned_if_correct_password_test() {
  TreasureChest("password", "treasure")
  |> treasure_chest.get_treasure("password")
  |> should.equal(Unlocked("treasure"))
}

pub fn wrong_password_is_returned_if_an_incorrect_password_is_used_test() {
  TreasureChest("password", "treasure")
  |> treasure_chest.get_treasure("wrong-password")
  |> should.equal(WrongPassword)
}
