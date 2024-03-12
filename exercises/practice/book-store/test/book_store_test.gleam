import book_store
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn only_a_single_book_test() {
  let input = [1]
  let output = book_store.lowest_price(input)
  let expected = 800.0
  output
  |> should.equal(expected)
}

pub fn two_of_the_same_book_test() {
  let input = [2, 2]
  let output = book_store.lowest_price(input)
  let expected = 1600.0
  output
  |> should.equal(expected)
}

pub fn empty_basket_test() {
  let input = []
  let output = book_store.lowest_price(input)
  let expected = 0.0
  output
  |> should.equal(expected)
}

pub fn two_different_books_test() {
  let input = [1, 2]
  let output = book_store.lowest_price(input)
  let expected = 1520.0
  output
  |> should.equal(expected)
}

pub fn three_different_books_test() {
  let input = [1, 2, 3]
  let output = book_store.lowest_price(input)
  let expected = 2160.0
  output
  |> should.equal(expected)
}

pub fn four_different_books_test() {
  let input = [1, 2, 3, 4]
  let output = book_store.lowest_price(input)
  let expected = 2560.0
  output
  |> should.equal(expected)
}

pub fn five_different_books_test() {
  let input = [1, 2, 3, 4, 5]
  let output = book_store.lowest_price(input)
  let expected = 3000.0
  output
  |> should.equal(expected)
}

pub fn two_groups_of_four_is_cheaper_than_group_of_five_plus_group_of_three_test() {
  let input = [1, 1, 2, 2, 3, 3, 4, 5]
  let output = book_store.lowest_price(input)
  let expected = 5120.0
  output
  |> should.equal(expected)
}

pub fn two_groups_of_four_is_cheaper_than_groups_of_five_and_three_test() {
  let input = [1, 1, 2, 3, 4, 4, 5, 5]
  let output = book_store.lowest_price(input)
  let expected = 5120.0
  output
  |> should.equal(expected)
}

pub fn group_of_four_plus_group_of_two_is_cheaper_than_two_groups_of_three_test() {
  let input = [1, 1, 2, 2, 3, 4]
  let output = book_store.lowest_price(input)
  let expected = 4080.0
  output
  |> should.equal(expected)
}

pub fn two_each_of_first_four_books_and_one_copy_each_of_rest_test() {
  let input = [1, 1, 2, 2, 3, 3, 4, 4, 5]
  let output = book_store.lowest_price(input)
  let expected = 5560.0
  output
  |> should.equal(expected)
}

pub fn two_copies_of_each_book_test() {
  let input = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5]
  let output = book_store.lowest_price(input)
  let expected = 6000.0
  output
  |> should.equal(expected)
}

pub fn three_copies_of_first_book_and_two_each_of_remaining_test() {
  let input = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 1]
  let output = book_store.lowest_price(input)
  let expected = 6800.0
  output
  |> should.equal(expected)
}

pub fn three_each_of_first_two_books_and_two_each_of_remaining_books_test() {
  let input = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 1, 2]
  let output = book_store.lowest_price(input)
  let expected = 7520.0
  output
  |> should.equal(expected)
}

pub fn four_groups_of_four_are_cheaper_than_two_groups_each_of_five_and_three_test() {
  let input = [1, 1, 2, 2, 3, 3, 4, 5, 1, 1, 2, 2, 3, 3, 4, 5]
  let output = book_store.lowest_price(input)
  let expected = 10_240.0
  output
  |> should.equal(expected)
}

pub fn check_that_groups_of_four_are_created_properly_even_when_there_are_more_groups_of_three_than_groups_of_five_test() {
  let input = [1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 5, 5]
  let output = book_store.lowest_price(input)
  let expected = 14_560.0
  output
  |> should.equal(expected)
}

pub fn one_group_of_one_and_four_is_cheaper_than_one_group_of_two_and_three_test() {
  let input = [1, 1, 2, 3, 4]
  let output = book_store.lowest_price(input)
  let expected = 3360.0
  output
  |> should.equal(expected)
}

pub fn one_group_of_one_and_two_plus_three_groups_of_four_is_cheaper_than_one_group_of_each_size_test() {
  let input = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5]
  let output = book_store.lowest_price(input)
  let expected = 10_000.0
  output
  |> should.equal(expected)
}
