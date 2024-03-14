import custom_set
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn returns_true_if_the_set_contains_no_elements_sets_with_no_elements_are_empty_test() {
  let set = custom_set.new([])

  let assert True = custom_set.is_empty(set)
}

pub fn returns_true_if_the_set_contains_no_elements_sets_with_elements_are_not_empty_test() {
  let set = custom_set.new([1])

  let assert False = custom_set.is_empty(set)
}

pub fn sets_can_report_if_they_contain_an_element_nothing_is_contained_in_an_empty_set_test() {
  let set = custom_set.new([])

  let assert False = custom_set.contains(in: set, this: 1)
}

pub fn sets_can_report_if_they_contain_an_element_when_the_element_is_in_the_set_test() {
  let set = custom_set.new([1, 2, 3])

  let assert True = custom_set.contains(in: set, this: 1)
}

pub fn sets_can_report_if_they_contain_an_element_when_the_element_is_not_in_the_set_test() {
  let set = custom_set.new([1, 2, 3])

  let assert False = custom_set.contains(in: set, this: 4)
}

pub fn a_set_is_a_subset_if_all_of_its_elements_are_contained_in_the_other_set_empty_set_is_a_subset_of_another_empty_set_test() {
  let set1 = custom_set.new([])
  let set2 = custom_set.new([])

  let assert True = custom_set.is_subset(set1, of: set2)
}

pub fn a_set_is_a_subset_if_all_of_its_elements_are_contained_in_the_other_set_empty_set_is_a_subset_of_non_empty_set_test() {
  let set1 = custom_set.new([])
  let set2 = custom_set.new([1])

  let assert True = custom_set.is_subset(set1, of: set2)
}

pub fn a_set_is_a_subset_if_all_of_its_elements_are_contained_in_the_other_set_non_empty_set_is_not_a_subset_of_empty_set_test() {
  let set1 = custom_set.new([1])
  let set2 = custom_set.new([])

  let assert False = custom_set.is_subset(set1, of: set2)
}

pub fn a_set_is_a_subset_if_all_of_its_elements_are_contained_in_the_other_set_set_is_a_subset_of_set_with_exact_same_elements_test() {
  let set1 = custom_set.new([1, 2, 3])
  let set2 = custom_set.new([1, 2, 3])

  let assert True = custom_set.is_subset(set1, of: set2)
}

pub fn a_set_is_a_subset_if_all_of_its_elements_are_contained_in_the_other_set_set_is_a_subset_of_larger_set_with_same_elements_test() {
  let set1 = custom_set.new([1, 2, 3])
  let set2 = custom_set.new([4, 1, 2, 3])

  let assert True = custom_set.is_subset(set1, of: set2)
}

pub fn a_set_is_a_subset_if_all_of_its_elements_are_contained_in_the_other_set_set_is_not_a_subset_of_set_that_does_not_contain_its_elements_test() {
  let set1 = custom_set.new([1, 2, 3])
  let set2 = custom_set.new([4, 1, 3])

  let assert False = custom_set.is_subset(set1, of: set2)
}

pub fn sets_are_disjoint_if_they_share_no_elements_the_empty_set_is_disjoint_with_itself_test() {
  let set1 = custom_set.new([])
  let set2 = custom_set.new([])

  let assert True = custom_set.disjoint(set1, set2)
}

pub fn sets_are_disjoint_if_they_share_no_elements_empty_set_is_disjoint_with_non_empty_set_test() {
  let set1 = custom_set.new([])
  let set2 = custom_set.new([1])

  let assert True = custom_set.disjoint(set1, set2)
}

pub fn sets_are_disjoint_if_they_share_no_elements_non_empty_set_is_disjoint_with_empty_set_test() {
  let set1 = custom_set.new([1])
  let set2 = custom_set.new([])

  let assert True = custom_set.disjoint(set1, set2)
}

pub fn sets_are_disjoint_if_they_share_no_elements_sets_are_not_disjoint_if_they_share_an_element_test() {
  let set1 = custom_set.new([1, 2])
  let set2 = custom_set.new([2, 3])

  let assert False = custom_set.disjoint(set1, set2)
}

pub fn sets_are_disjoint_if_they_share_no_elements_sets_are_disjoint_if_they_share_no_elements_test() {
  let set1 = custom_set.new([1, 2])
  let set2 = custom_set.new([3, 4])

  let assert True = custom_set.disjoint(set1, set2)
}

pub fn sets_with_the_same_elements_are_equal_empty_sets_are_equal_test() {
  let set1 = custom_set.new([])
  let set2 = custom_set.new([])

  let assert True = custom_set.is_equal(set1, to: set2)
}

pub fn sets_with_the_same_elements_are_equal_empty_set_is_not_equal_to_non_empty_set_test() {
  let set1 = custom_set.new([])
  let set2 = custom_set.new([1, 2, 3])

  let assert False = custom_set.is_equal(set1, to: set2)
}

pub fn sets_with_the_same_elements_are_equal_non_empty_set_is_not_equal_to_empty_set_test() {
  let set1 = custom_set.new([1, 2, 3])
  let set2 = custom_set.new([])

  let assert False = custom_set.is_equal(set1, to: set2)
}

pub fn sets_with_the_same_elements_are_equal_sets_with_the_same_elements_are_equal_test() {
  let set1 = custom_set.new([1, 2])
  let set2 = custom_set.new([2, 1])

  let assert True = custom_set.is_equal(set1, to: set2)
}

pub fn sets_with_the_same_elements_are_equal_sets_with_different_elements_are_not_equal_test() {
  let set1 = custom_set.new([1, 2, 3])
  let set2 = custom_set.new([1, 2, 4])

  let assert False = custom_set.is_equal(set1, to: set2)
}

pub fn sets_with_the_same_elements_are_equal_set_is_not_equal_to_larger_set_with_same_elements_test() {
  let set1 = custom_set.new([1, 2, 3])
  let set2 = custom_set.new([1, 2, 3, 4])

  let assert False = custom_set.is_equal(set1, to: set2)
}

pub fn unique_elements_can_be_added_to_a_set_add_to_empty_set_test() {
  let set = custom_set.new([])

  let actual = custom_set.add(to: set, this: 3)
  let expected = custom_set.new([3])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn unique_elements_can_be_added_to_a_set_add_to_non_empty_set_test() {
  let set = custom_set.new([1, 2, 4])

  let actual = custom_set.add(to: set, this: 3)
  let expected = custom_set.new([1, 2, 3, 4])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn unique_elements_can_be_added_to_a_set_adding_an_existing_element_does_not_change_the_set_test() {
  let set = custom_set.new([1, 2, 3])

  let actual = custom_set.add(to: set, this: 3)
  let expected = custom_set.new([1, 2, 3])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn intersection_returns_a_set_of_all_shared_elements_intersection_of_two_empty_sets_is_an_empty_set_test() {
  let set1 = custom_set.new([])
  let set2 = custom_set.new([])

  let actual = custom_set.intersection(of: set1, and: set2)
  let expected = custom_set.new([])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn intersection_returns_a_set_of_all_shared_elements_intersection_of_an_empty_set_and_non_empty_set_is_an_empty_set_test() {
  let set1 = custom_set.new([])
  let set2 = custom_set.new([3, 2, 5])

  let actual = custom_set.intersection(of: set1, and: set2)
  let expected = custom_set.new([])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn intersection_returns_a_set_of_all_shared_elements_intersection_of_a_non_empty_set_and_an_empty_set_is_an_empty_set_test() {
  let set1 = custom_set.new([1, 2, 3, 4])
  let set2 = custom_set.new([])

  let actual = custom_set.intersection(of: set1, and: set2)
  let expected = custom_set.new([])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn intersection_returns_a_set_of_all_shared_elements_intersection_of_two_sets_with_no_shared_elements_is_an_empty_set_test() {
  let set1 = custom_set.new([1, 2, 3])
  let set2 = custom_set.new([4, 5, 6])

  let actual = custom_set.intersection(of: set1, and: set2)
  let expected = custom_set.new([])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn intersection_returns_a_set_of_all_shared_elements_intersection_of_two_sets_with_shared_elements_is_a_set_of_the_shared_elements_test() {
  let set1 = custom_set.new([1, 2, 3, 4])
  let set2 = custom_set.new([3, 2, 5])

  let actual = custom_set.intersection(of: set1, and: set2)
  let expected = custom_set.new([2, 3])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn difference_or_complement_of_a_set_is_a_set_of_all_elements_that_are_only_in_the_first_set_difference_of_two_empty_sets_is_an_empty_set_test() {
  let set1 = custom_set.new([])
  let set2 = custom_set.new([])

  let actual = custom_set.difference(between: set1, and: set2)
  let expected = custom_set.new([])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn difference_or_complement_of_a_set_is_a_set_of_all_elements_that_are_only_in_the_first_set_difference_of_empty_set_and_non_empty_set_is_an_empty_set_test() {
  let set1 = custom_set.new([])
  let set2 = custom_set.new([3, 2, 5])

  let actual = custom_set.difference(between: set1, and: set2)
  let expected = custom_set.new([])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn difference_or_complement_of_a_set_is_a_set_of_all_elements_that_are_only_in_the_first_set_difference_of_a_non_empty_set_and_an_empty_set_is_the_non_empty_set_test() {
  let set1 = custom_set.new([1, 2, 3, 4])
  let set2 = custom_set.new([])

  let actual = custom_set.difference(between: set1, and: set2)
  let expected = custom_set.new([1, 2, 3, 4])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn difference_or_complement_of_a_set_is_a_set_of_all_elements_that_are_only_in_the_first_set_difference_of_two_non_empty_sets_is_a_set_of_elements_that_are_only_in_the_first_set_test() {
  let set1 = custom_set.new([3, 2, 1])
  let set2 = custom_set.new([2, 4])

  let actual = custom_set.difference(between: set1, and: set2)
  let expected = custom_set.new([1, 3])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn union_returns_a_set_of_all_elements_in_either_set_union_of_empty_sets_is_an_empty_set_test() {
  let set1 = custom_set.new([])
  let set2 = custom_set.new([])

  let actual = custom_set.union(of: set1, and: set2)
  let expected = custom_set.new([])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn union_returns_a_set_of_all_elements_in_either_set_union_of_an_empty_set_and_non_empty_set_is_the_non_empty_set_test() {
  let set1 = custom_set.new([])
  let set2 = custom_set.new([2])

  let actual = custom_set.union(of: set1, and: set2)
  let expected = custom_set.new([2])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn union_returns_a_set_of_all_elements_in_either_set_union_of_a_non_empty_set_and_empty_set_is_the_non_empty_set_test() {
  let set1 = custom_set.new([1, 3])
  let set2 = custom_set.new([])

  let actual = custom_set.union(of: set1, and: set2)
  let expected = custom_set.new([1, 3])

  let assert True = custom_set.is_equal(actual, expected)
}

pub fn union_returns_a_set_of_all_elements_in_either_set_union_of_non_empty_sets_contains_all_unique_elements_test() {
  let set1 = custom_set.new([1, 3])
  let set2 = custom_set.new([2, 3])

  let actual = custom_set.union(of: set1, and: set2)
  let expected = custom_set.new([3, 2, 1])

  let assert True = custom_set.is_equal(actual, expected)
}
