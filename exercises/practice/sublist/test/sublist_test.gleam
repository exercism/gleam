import exercism/should
import exercism/test_runner
import sublist.{Equal, Sublist, Superlist, Unequal}

pub fn main() {
  test_runner.main()
}

pub fn empty_lists_test() {
  sublist.sublist(compare: [], to: [])
  |> should.equal(Equal)
}

pub fn empty_list_within_non_empty_list_test() {
  sublist.sublist(compare: [], to: [1, 2, 3])
  |> should.equal(Sublist)
}

pub fn non_empty_list_contains_empty_list_test() {
  sublist.sublist(compare: [1, 2, 3], to: [])
  |> should.equal(Superlist)
}

pub fn list_equals_itself_test() {
  sublist.sublist(compare: [1, 2, 3], to: [1, 2, 3])
  |> should.equal(Equal)
}

pub fn different_lists_test() {
  sublist.sublist(compare: [1, 2, 3], to: [2, 3, 4])
  |> should.equal(Unequal)
}

pub fn false_start_test() {
  sublist.sublist(compare: [1, 2, 5], to: [0, 1, 2, 3, 1, 2, 5, 6])
  |> should.equal(Sublist)
}

pub fn consecutive_test() {
  sublist.sublist(compare: [1, 1, 2], to: [0, 1, 1, 1, 2, 1, 2])
  |> should.equal(Sublist)
}

pub fn sublist_at_start_test() {
  sublist.sublist(compare: [0, 1, 2], to: [0, 1, 2, 3, 4, 5])
  |> should.equal(Sublist)
}

pub fn sublist_in_middle_test() {
  sublist.sublist(compare: [2, 3, 4], to: [0, 1, 2, 3, 4, 5])
  |> should.equal(Sublist)
}

pub fn sublist_at_end_test() {
  sublist.sublist(compare: [3, 4, 5], to: [0, 1, 2, 3, 4, 5])
  |> should.equal(Sublist)
}

pub fn at_start_of_superlist_test() {
  sublist.sublist(compare: [0, 1, 2, 3, 4, 5], to: [0, 1, 2])
  |> should.equal(Superlist)
}

pub fn in_middle_of_superlist_test() {
  sublist.sublist(compare: [0, 1, 2, 3, 4, 5], to: [2, 3])
  |> should.equal(Superlist)
}

pub fn at_end_of_superlist_test() {
  sublist.sublist(compare: [0, 1, 2, 3, 4, 5], to: [3, 4, 5])
  |> should.equal(Superlist)
}

pub fn first_list_missing_element_from_second_list_test() {
  sublist.sublist(compare: [1, 3], to: [1, 2, 3])
  |> should.equal(Unequal)
}

pub fn second_list_missing_element_from_first_list_test() {
  sublist.sublist(compare: [1, 2, 3], to: [1, 3])
  |> should.equal(Unequal)
}

pub fn first_list_missing_additional_digits_from_second_list_test() {
  sublist.sublist(compare: [1, 2], to: [1, 22])
  |> should.equal(Unequal)
}

pub fn order_matters_to_a_list_test() {
  sublist.sublist(compare: [1, 2, 3], to: [3, 2, 1])
  |> should.equal(Unequal)
}

pub fn same_digits_but_different_numbers_test() {
  sublist.sublist(compare: [1, 0, 1], to: [10, 1])
  |> should.equal(Unequal)
}
