import exercism/should
import exercism/test_runner
import list_ops

pub fn main() {
  test_runner.main()
}

pub fn append_entries_to_a_list_and_return_the_new_list_empty_lists_test() {
  list_ops.append(first: [], second: [])
  |> should.equal([])
}

pub fn append_entries_to_a_list_and_return_the_new_list_list_to_empty_list_test() {
  list_ops.append(first: [], second: [1, 2, 3, 4])
  |> should.equal([1, 2, 3, 4])
}

pub fn append_entries_to_a_list_and_return_the_new_list_empty_list_to_list_test() {
  list_ops.append(first: [1, 2, 3, 4], second: [])
  |> should.equal([1, 2, 3, 4])
}

pub fn append_entries_to_a_list_and_return_the_new_list_non_empty_lists_test() {
  list_ops.append(first: [1, 2], second: [2, 3, 4, 5])
  |> should.equal([1, 2, 2, 3, 4, 5])
}

pub fn concatenate_a_list_of_lists_empty_list_test() {
  list_ops.concat([])
  |> should.equal([])
}

pub fn concatenate_a_list_of_lists_list_of_lists_test() {
  list_ops.concat([[1, 2], [3], [], [4, 5, 6]])
  |> should.equal([1, 2, 3, 4, 5, 6])
}

pub fn concatenate_a_list_of_lists_list_of_nested_lists_test() {
  list_ops.concat([[[1], [2]], [[3]], [[]], [[4, 5, 6]]])
  |> should.equal([[1], [2], [3], [], [4, 5, 6]])
}

pub fn filter_list_returning_only_values_that_satisfy_the_filter_function_empty_list_test() {
  list_ops.filter([], fn(x) { x % 2 == 1 })
  |> should.equal([])
}

pub fn filter_list_returning_only_values_that_satisfy_the_filter_function_non_empty_list_test() {
  list_ops.filter([1, 2, 3, 5], fn(x) { x % 2 == 1 })
  |> should.equal([1, 3, 5])
}

pub fn returns_the_length_of_a_list_empty_list_test() {
  list_ops.length([])
  |> should.equal(0)
}

pub fn returns_the_length_of_a_list_non_empty_list_test() {
  list_ops.length([1, 2, 3, 4])
  |> should.equal(4)
}

pub fn return_a_list_of_elements_whose_values_equal_the_list_value_transformed_by_the_mapping_function_empty_list_test() {
  list_ops.map([], fn(x) { x + 1 })
  |> should.equal([])
}

pub fn return_a_list_of_elements_whose_values_equal_the_list_value_transformed_by_the_mapping_function_non_empty_list_test() {
  list_ops.map([1, 3, 5, 7], fn(x) { x + 1 })
  |> should.equal([2, 4, 6, 8])
}

pub fn folds_reduces_the_given_list_from_the_left_with_a_function_empty_list_test() {
  list_ops.foldl(with: fn(acc, el) { el * acc }, from: 2, over: [])
  |> should.equal(2)
}

pub fn folds_reduces_the_given_list_from_the_left_with_a_function_direction_independent_function_applied_to_non_empty_list_test() {
  list_ops.foldl(with: fn(acc, el) { el + acc }, from: 5, over: [1, 2, 3, 4])
  |> should.equal(15)
}

pub fn folds_reduces_the_given_list_from_the_left_with_a_function_direction_dependent_function_applied_to_non_empty_list_test() {
  list_ops.foldl(with: fn(acc, el) { el /. acc }, from: 24.0, over: [
    1.0, 2.0, 3.0, 4.0,
  ])
  |> should.equal(64.0)
}

pub fn folds_reduces_the_given_list_from_the_right_with_a_function_empty_list_test() {
  list_ops.foldr(with: fn(acc, el) { el * acc }, from: 2, over: [])
  |> should.equal(2)
}

pub fn folds_reduces_the_given_list_from_the_right_with_a_function_direction_independent_function_applied_to_non_empty_list_test() {
  list_ops.foldr(with: fn(acc, el) { el + acc }, from: 5, over: [1, 2, 3, 4])
  |> should.equal(15)
}

pub fn folds_reduces_the_given_list_from_the_right_with_a_function_direction_dependent_function_applied_to_non_empty_list_test() {
  list_ops.foldr(with: fn(acc, el) { el /. acc }, from: 24.0, over: [
    1.0, 2.0, 3.0, 4.0,
  ])
  |> should.equal(9.0)
}

pub fn reverse_the_elements_of_the_list_empty_list_test() {
  list_ops.reverse([])
  |> should.equal([])
}

pub fn reverse_the_elements_of_the_list_non_empty_list_test() {
  list_ops.reverse([1, 3, 5, 7])
  |> should.equal([7, 5, 3, 1])
}

pub fn reverse_the_elements_of_the_list_list_of_lists_is_not_flattened_test() {
  list_ops.reverse([[1, 2], [3], [], [4, 5, 6]])
  |> should.equal([[4, 5, 6], [], [3], [1, 2]])
}
