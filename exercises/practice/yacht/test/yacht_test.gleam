import exercism/test_runner
import exercism/should
import yacht

pub fn main() {
  test_runner.main()
}

pub fn ones_test() {
  yacht.score(yacht.Ones, [1, 1, 1, 3, 5])
  |> should.equal(3)
}

pub fn ones_out_of_order_test() {
  yacht.score(yacht.Ones, [3, 1, 1, 5, 1])
  |> should.equal(3)
}

pub fn no_ones_test() {
  yacht.score(yacht.Ones, [4, 3, 6, 5, 5])
  |> should.equal(0)
}

pub fn twos_test() {
  yacht.score(yacht.Twos, [2, 3, 4, 5, 6])
  |> should.equal(2)
}

pub fn yacht_counted_as_threes_test() {
  yacht.score(yacht.Threes, [3, 3, 3, 3, 3])
  |> should.equal(15)
}

pub fn fours_test() {
  yacht.score(yacht.Fours, [1, 4, 1, 4, 1])
  |> should.equal(8)
}

pub fn yacht_of_3s_counted_as_fives_test() {
  yacht.score(yacht.Fives, [3, 3, 3, 3, 3])
  |> should.equal(0)
}

pub fn fives_test() {
  yacht.score(yacht.Fives, [1, 5, 3, 5, 3])
  |> should.equal(10)
}

pub fn sixes_test() {
  yacht.score(yacht.Sixes, [2, 3, 4, 5, 6])
  |> should.equal(6)
}

pub fn full_house_two_small_three_big_test() {
  yacht.score(yacht.FullHouse, [2, 2, 4, 4, 4])
  |> should.equal(16)
}

pub fn full_house_three_small_two_big_test() {
  yacht.score(yacht.FullHouse, [5, 3, 3, 5, 3])
  |> should.equal(19)
}

pub fn two_pair_is_not_a_full_house_test() {
  yacht.score(yacht.FullHouse, [2, 2, 4, 4, 5])
  |> should.equal(0)
}

pub fn four_of_a_kind_is_not_a_full_house_test() {
  yacht.score(yacht.FullHouse, [1, 4, 4, 4, 4])
  |> should.equal(0)
}

pub fn yacht_is_not_a_full_house_test() {
  yacht.score(yacht.FullHouse, [2, 2, 2, 2, 2])
  |> should.equal(0)
}

pub fn four_of_a_kind_test() {
  yacht.score(yacht.FourOfAKind, [6, 6, 4, 6, 6])
  |> should.equal(24)
}

pub fn yacht_can_be_scored_as_four_of_a_kind_test() {
  yacht.score(yacht.FourOfAKind, [3, 3, 3, 3, 3])
  |> should.equal(12)
}

pub fn full_house_is_not_four_of_a_kind_test() {
  yacht.score(yacht.FourOfAKind, [3, 3, 3, 5, 5])
  |> should.equal(0)
}

pub fn little_straight_test() {
  yacht.score(yacht.LittleStraight, [3, 5, 4, 1, 2])
  |> should.equal(30)
}

pub fn little_straight_as_big_straight_test() {
  yacht.score(yacht.BigStraight, [1, 2, 3, 4, 5])
  |> should.equal(0)
}

pub fn four_in_order_but_not_a_little_straight_test() {
  yacht.score(yacht.LittleStraight, [1, 1, 2, 3, 4])
  |> should.equal(0)
}

pub fn no_pairs_but_not_a_little_straight_test() {
  yacht.score(yacht.LittleStraight, [1, 2, 3, 4, 6])
  |> should.equal(0)
}

pub fn minimum_is_1_maximum_is_5_but_not_a_little_straight_test() {
  yacht.score(yacht.LittleStraight, [1, 1, 3, 4, 5])
  |> should.equal(0)
}

pub fn big_straight_test() {
  yacht.score(yacht.BigStraight, [4, 6, 2, 5, 3])
  |> should.equal(30)
}

pub fn big_straight_as_little_straight_test() {
  yacht.score(yacht.LittleStraight, [6, 5, 4, 3, 2])
  |> should.equal(0)
}

pub fn no_pairs_but_not_a_big_straight_test() {
  yacht.score(yacht.BigStraight, [6, 5, 4, 3, 1])
  |> should.equal(0)
}

pub fn choice_test() {
  yacht.score(yacht.Choice, [3, 3, 5, 6, 6])
  |> should.equal(23)
}

pub fn yacht_as_choice_test() {
  yacht.score(yacht.Choice, [2, 2, 2, 2, 2])
  |> should.equal(10)
}

pub fn yacht_test() {
  yacht.score(yacht.Yacht, [5, 5, 5, 5, 5])
  |> should.equal(50)
}

pub fn not_yacht_test() {
  yacht.score(yacht.Yacht, [1, 3, 3, 2, 5])
  |> should.equal(0)
}
