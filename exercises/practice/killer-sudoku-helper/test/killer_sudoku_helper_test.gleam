import exercism/should
import exercism/test_runner
import killer_sudoku_helper

pub fn main() {
  test_runner.main()
}

pub fn trivial_1_digit_cages_1_test() {
  killer_sudoku_helper.combinations(size: 1, sum: 1, exclude: [])
  |> should.equal([[1]])
}

pub fn trivial_1_digit_cages_2_test() {
  killer_sudoku_helper.combinations(size: 1, sum: 2, exclude: [])
  |> should.equal([[2]])
}

pub fn trivial_1_digit_cages_3_test() {
  killer_sudoku_helper.combinations(size: 1, sum: 3, exclude: [])
  |> should.equal([[3]])
}

pub fn trivial_1_digit_cages_4_test() {
  killer_sudoku_helper.combinations(size: 1, sum: 4, exclude: [])
  |> should.equal([[4]])
}

pub fn trivial_1_digit_cages_5_test() {
  killer_sudoku_helper.combinations(size: 1, sum: 5, exclude: [])
  |> should.equal([[5]])
}

pub fn trivial_1_digit_cages_6_test() {
  killer_sudoku_helper.combinations(size: 1, sum: 6, exclude: [])
  |> should.equal([[6]])
}

pub fn trivial_1_digit_cages_7_test() {
  killer_sudoku_helper.combinations(size: 1, sum: 7, exclude: [])
  |> should.equal([[7]])
}

pub fn trivial_1_digit_cages_8_test() {
  killer_sudoku_helper.combinations(size: 1, sum: 8, exclude: [])
  |> should.equal([[8]])
}

pub fn trivial_1_digit_cages_9_test() {
  killer_sudoku_helper.combinations(size: 1, sum: 9, exclude: [])
  |> should.equal([[9]])
}

pub fn cage_with_sum_45_contains_all_digits_1_9_test() {
  killer_sudoku_helper.combinations(size: 9, sum: 45, exclude: [])
  |> should.equal([[1, 2, 3, 4, 5, 6, 7, 8, 9]])
}

pub fn cage_with_only_1_possible_combination_test() {
  killer_sudoku_helper.combinations(size: 3, sum: 7, exclude: [])
  |> should.equal([[1, 2, 4]])
}

pub fn cage_with_several_combinations_test() {
  killer_sudoku_helper.combinations(size: 2, sum: 10, exclude: [])
  |> should.equal([[1, 9], [2, 8], [3, 7], [4, 6]])
}

pub fn cage_with_several_combinations_that_is_restricted_test() {
  killer_sudoku_helper.combinations(size: 2, sum: 10, exclude: [1, 4])
  |> should.equal([[2, 8], [3, 7]])
}
