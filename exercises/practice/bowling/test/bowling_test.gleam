import bowling.{
  type Error, Game, GameComplete, GameNotComplete, InvalidPinCount, roll, score,
}
import exercism/should
import exercism/test_runner
import gleam/list

// Students should implement roll and score methods.
// Roll should accept a single integer.
// Score should return the game's final score, when possible
// For brevity the tests display all the previous rolls in an array;
// each element of the previousRolls array should be passed to the roll method
// and each of those previous rolls is expected to succeed.
// 
// Two properties are tested:
// 
// `score`: All rolls succeed, and taking the score gives the expected result.
// The expected result may be an integer score or an error.
// 
// `roll`: All previousRolls succeed, and rolling the number of pins in `roll` produces the expected result.
// Currently, all cases of this type result in errors.
// 
// In all error cases you should expect an error as is idiomatic for your language
// whether that be via exceptions, optional values, or otherwise.

pub fn main() {
  test_runner.main()
}

pub fn should_be_able_to_score_a_game_with_all_zeros_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  |> roll_and_check_score(0)
}

pub fn should_be_able_to_score_a_game_with_no_strikes_or_spares_test() {
  [3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6]
  |> roll_and_check_score(90)
}

pub fn a_spare_followed_by_zeros_is_worth_ten_points_test() {
  [6, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  |> roll_and_check_score(10)
}

pub fn points_scored_in_the_roll_after_a_spare_are_counted_twice_test() {
  [6, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  |> roll_and_check_score(16)
}

pub fn consecutive_spares_each_get_a_one_roll_bonus_test() {
  [5, 5, 3, 7, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  |> roll_and_check_score(31)
}

pub fn a_spare_in_the_last_frame_gets_a_one_roll_bonus_that_is_counted_once_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 7]
  |> roll_and_check_score(17)
}

pub fn a_strike_earns_ten_points_in_a_frame_with_a_single_roll_test() {
  [10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  |> roll_and_check_score(10)
}

pub fn points_scored_in_the_two_rolls_after_a_strike_are_counted_twice_as_a_bonus_test() {
  [10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  |> roll_and_check_score(26)
}

pub fn consecutive_strikes_each_get_the_two_roll_bonus_test() {
  [10, 10, 10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  |> roll_and_check_score(81)
}

pub fn a_strike_in_the_last_frame_gets_a_two_roll_bonus_that_is_counted_once_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 7, 1]
  |> roll_and_check_score(18)
}

pub fn rolling_a_spare_with_the_two_roll_bonus_does_not_get_a_bonus_roll_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 7, 3]
  |> roll_and_check_score(20)
}

pub fn strikes_with_the_two_roll_bonus_do_not_get_bonus_rolls_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10]
  |> roll_and_check_score(30)
}

pub fn last_two_strikes_followed_by_only_last_bonus_with_non_strike_points_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 0, 1]
  |> roll_and_check_score(31)
}

pub fn a_strike_with_the_one_roll_bonus_after_a_spare_in_the_last_frame_does_not_get_a_bonus_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 10]
  |> roll_and_check_score(20)
}

pub fn all_strikes_is_a_perfect_game_test() {
  [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]
  |> roll_and_check_score(300)
}

pub fn rolls_cannot_score_negative_points_test() {
  let assert Error(_) = roll(Game([]), -1)
}

pub fn a_roll_cannot_score_more_than_10_points_test() {
  let assert Error(_) = roll(Game([]), -1)
}

pub fn two_rolls_in_a_frame_cannot_score_more_than_10_points_test() {
  let assert Ok(game) = roll(Game([]), 5)
  let assert Error(_) = roll(game, 6)
}

pub fn bonus_roll_after_a_strike_in_the_last_frame_cannot_score_more_than_10_points_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
  |> roll_and_last_roll_be_error(11, InvalidPinCount)
}

pub fn two_bonus_rolls_after_a_strike_in_the_last_frame_cannot_score_more_than_10_points_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 5]
  |> roll_and_last_roll_be_error(6, InvalidPinCount)
}

pub fn two_bonus_rolls_after_a_strike_in_the_last_frame_can_score_more_than_10_points_if_one_is_a_strike_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 6]
  |> roll_and_check_score(26)
}

pub fn the_second_bonus_rolls_after_a_strike_in_the_last_frame_cannot_be_a_strike_if_the_first_one_is_not_a_strike_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 6]
  |> roll_and_last_roll_be_error(10, InvalidPinCount)
}

pub fn second_bonus_roll_after_a_strike_in_the_last_frame_cannot_score_more_than_10_points_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10]
  |> roll_and_last_roll_be_error(11, InvalidPinCount)
}

pub fn an_unstarted_game_cannot_be_scored_test() {
  []
  |> roll_and_score_be_error()
}

pub fn an_incomplete_game_cannot_be_scored_test() {
  [0, 0]
  |> roll_and_score_be_error()
}

pub fn cannot_roll_if_game_already_has_ten_frames_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  |> roll_and_last_roll_be_error(0, GameComplete)
}

pub fn bonus_rolls_for_a_strike_in_the_last_frame_must_be_rolled_before_score_can_be_calculated_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10]
  |> roll_and_score_be_error()
}

pub fn both_bonus_rolls_for_a_strike_in_the_last_frame_must_be_rolled_before_score_can_be_calculated_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10]
  |> roll_and_score_be_error()
}

pub fn bonus_roll_for_a_spare_in_the_last_frame_must_be_rolled_before_score_can_be_calculated_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3]
  |> roll_and_score_be_error()
}

pub fn cannot_roll_after_bonus_roll_for_spare_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 2]
  |> roll_and_last_roll_be_error(2, GameComplete)
}

pub fn cannot_roll_after_bonus_rolls_for_strike_test() {
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 3, 2]
  |> roll_and_last_roll_be_error(2, GameComplete)
}

fn roll_and_check_score(rolls: List(Int), correct_score: Int) {
  rolls
  |> list.fold(Game([]), fn(game, pins) {
    let assert Ok(new_game) = roll(game, pins)
    new_game
  })
  |> score()
  |> should.equal(Ok(correct_score))
}

fn roll_and_last_roll_be_error(rolls: List(Int), last_roll: Int, error: Error) {
  rolls
  |> list.fold(Game([]), fn(game, pins) {
    let assert Ok(new_game) = roll(game, pins)
    new_game
  })
  |> roll(last_roll)
  |> should.equal(Error(error))
}

fn roll_and_score_be_error(rolls: List(Int)) {
  rolls
  |> list.fold(Game([]), fn(game, pins) {
    let assert Ok(new_game) = roll(game, pins)
    new_game
  })
  |> score()
  |> should.equal(Error(GameNotComplete))
}
