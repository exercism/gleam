import change.{ImpossibleTarget}
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn change_for_1_cent_test() {
  change.find_fewest_coins([1, 5, 10, 25], 1)
  |> should.equal(Ok([1]))
}

pub fn single_coin_change_test() {
  change.find_fewest_coins([1, 5, 10, 25, 100], 25)
  |> should.equal(Ok([25]))
}

pub fn multiple_coin_change_test() {
  change.find_fewest_coins([1, 5, 10, 25, 100], 15)
  |> should.equal(Ok([5, 10]))
}

pub fn change_with_lilliputian_coins_test() {
  change.find_fewest_coins([1, 4, 15, 20, 50], 23)
  |> should.equal(Ok([4, 4, 15]))
}

pub fn change_with_lower_elbonia_coins_test() {
  change.find_fewest_coins([1, 5, 10, 21, 25], 63)
  |> should.equal(Ok([21, 21, 21]))
}

pub fn large_target_values_test() {
  change.find_fewest_coins([1, 2, 5, 10, 20, 50, 100], 999)
  |> should.equal(
    Ok([2, 2, 5, 20, 20, 50, 100, 100, 100, 100, 100, 100, 100, 100, 100]),
  )
}

pub fn possible_change_without_unit_coins_available_test() {
  change.find_fewest_coins([2, 5, 10, 20, 50], 21)
  |> should.equal(Ok([2, 2, 2, 5, 10]))
}

pub fn another_possible_change_without_unit_coins_available_test() {
  change.find_fewest_coins([4, 5], 27)
  |> should.equal(Ok([4, 4, 4, 5, 5, 5]))
}

pub fn no_coins_make_0_change_test() {
  change.find_fewest_coins([1, 5, 10, 21, 25], 0)
  |> should.equal(Ok([]))
}

pub fn error_testing_for_change_smaller_than_the_smallest_of_coins_test() {
  change.find_fewest_coins([5, 10], 3)
  |> should.equal(Error(ImpossibleTarget))
}

pub fn error_if_no_combination_can_add_up_to_target_test() {
  change.find_fewest_coins([5, 10], 94)
  |> should.equal(Error(ImpossibleTarget))
}

pub fn cannot_find_negative_change_values_test() {
  change.find_fewest_coins([1, 2, 5], -5)
  |> should.equal(Error(ImpossibleTarget))
}
