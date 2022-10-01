import bowling.{
  Error, Game, GameComplete, GameNotComplete, InvalidPinCount, roll, score,
}
import gleeunit
import gleeunit/should
import gleam/list

pub fn main() {
  gleeunit.main()
}

pub fn all_zeroes_score_test() {
  let rolls = list.repeat(0, times: 20)

  rolls
  |> roll_and_check_score(0)
}

pub fn no_stirkes_or_spares_test() {
  let rolls = [3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6]

  rolls
  |> roll_and_check_score(90)
}

pub fn spare_followed_by_zeroes_test() {
  let rolls = [6, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

  rolls
  |> roll_and_check_score(10)
}

pub fn points_after_spare_test() {
  let rolls = [6, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

  rolls
  |> roll_and_check_score(16)
}

pub fn consecutive_spares_test() {
  let rolls = [5, 5, 3, 7, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

  rolls
  |> roll_and_check_score(31)
}

pub fn spare_in_last_frame_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 7]

  rolls
  |> roll_and_check_score(17)
}

pub fn strike_in_single_roll_frame_test() {
  let rolls = [10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

  rolls
  |> roll_and_check_score(10)
}

pub fn two_rolls_after_strike_test() {
  let rolls = [10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

  rolls
  |> roll_and_check_score(26)
}

pub fn consecutive_strikes_test() {
  let rolls = [10, 10, 10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

  rolls
  |> roll_and_check_score(81)
}

pub fn strike_in_last_frame_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 7, 1]

  rolls
  |> roll_and_check_score(18)
}

pub fn spare_with_two_roll_bonus_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 7, 3]

  rolls
  |> roll_and_check_score(20)
}

pub fn strike_with_two_roll_bonus_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10]

  rolls
  |> roll_and_check_score(30)
}

pub fn strike_with_one_roll_bonus_after_spare_in_last_frame_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 10]

  rolls
  |> roll_and_check_score(20)
}

pub fn all_strikes_test() {
  let rolls = [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]

  rolls
  |> roll_and_check_score(300)
}

pub fn negative_point_roll_test() {
  Game([])
  |> roll(-1)
  |> should.be_error()
}

pub fn more_than_10_points_roll_test() {
  Game([])
  |> roll(11)
  |> should.be_error()
}

pub fn more_than_10_points_frame_test() {
  assert Ok(game) =
    Game([])
    |> roll(5)

  game
  |> roll(6)
  |> should.be_error()
}

pub fn bonus_roll_after_strike_in_last_frame_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10]

  rolls
  |> roll_and_last_roll_be_error(11, InvalidPinCount)
}

pub fn two_bonus_rolls_after_strike_in_last_frame_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 5]

  rolls
  |> roll_and_last_roll_be_error(6, InvalidPinCount)
}

pub fn two_bonus_rolls_after_strike_in_last_frame_and_one_is_strike_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 6]

  rolls
  |> roll_and_check_score(26)
}

pub fn two_bonus_rolls_after_strike_in_last_frame_and_first_one_is_not_strike_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 6]

  rolls
  |> roll_and_last_roll_be_error(10, InvalidPinCount)
}

pub fn two_bonus_rolls_after_strike_in_last_frame_and_first_one_is_strike_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10]

  rolls
  |> roll_and_last_roll_be_error(11, InvalidPinCount)
}

pub fn trying_to_score_unstarted_game_test() {
  Game([])
  |> score()
  |> should.be_error()
}

pub fn trying_to_score_incomplete_game_test() {
  let rolls = [0, 0]

  rolls
  |> roll_and_score_be_error()
}

pub fn rolling_after_10_frames_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

  rolls
  |> roll_and_last_roll_be_error(0, GameComplete)
}

pub fn trying_to_score_game_before_rolling_bonus_rolls_after_strike_in_last_frame_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10]

  rolls
  |> roll_and_score_be_error()
}

pub fn trying_to_score_game_before_rolling_both_bonus_rolls_after_strike_in_last_frame_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10]

  rolls
  |> roll_and_score_be_error()
}

pub fn trying_to_score_game_before_rolling_bonus_roll_after_spare_in_last_frame_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3]

  rolls
  |> roll_and_score_be_error()
}

pub fn rolling_after_bonus_roll_after_spare_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 2]

  rolls
  |> roll_and_last_roll_be_error(2, GameComplete)
}

pub fn rolling_after_bonus_rolls_after_strike_test() {
  let rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 3, 2]

  rolls
  |> roll_and_last_roll_be_error(2, GameComplete)
}

fn roll_and_check_score(rolls: List(Int), correct_score: Int) {
  rolls
  |> list.fold(
    Game([]),
    fn(game, pins) {
      assert Ok(new_game) = roll(game, pins)
      new_game
    },
  )
  |> score()
  |> should.equal(Ok(correct_score))
}

fn roll_and_last_roll_be_error(rolls: List(Int), last_roll: Int, error: Error) {
  rolls
  |> list.fold(
    Game([]),
    fn(game, pins) {
      assert Ok(new_game) = roll(game, pins)
      new_game
    },
  )
  |> roll(last_roll)
  |> should.equal(Error(error))
}

fn roll_and_score_be_error(rolls: List(Int)) {
  rolls
  |> list.fold(
    Game([]),
    fn(game, pins) {
      assert Ok(new_game) = roll(game, pins)
      new_game
    },
  )
  |> score()
  |> should.equal(Error(GameNotComplete))
}
