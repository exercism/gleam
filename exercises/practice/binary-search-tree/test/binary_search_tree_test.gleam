import binary_search_tree.{Nil, Node}
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn data_is_retained_test() {
  binary_search_tree.to_tree([4])
  |> should.equal(Node(data: 4, left: Nil, right: Nil))
}

pub fn insert_data_at_proper_node_smaller_number_at_left_node_test() {
  binary_search_tree.to_tree([4, 2])
  |> should.equal(Node(
    data: 4,
    left: Node(data: 2, left: Nil, right: Nil),
    right: Nil,
  ))
}

pub fn insert_data_at_proper_node_same_number_at_left_node_test() {
  binary_search_tree.to_tree([4, 4])
  |> should.equal(Node(
    data: 4,
    left: Node(data: 4, left: Nil, right: Nil),
    right: Nil,
  ))
}

pub fn insert_data_at_proper_node_greater_number_at_right_node_test() {
  binary_search_tree.to_tree([4, 5])
  |> should.equal(Node(
    data: 4,
    left: Nil,
    right: Node(data: 5, left: Nil, right: Nil),
  ))
}

pub fn can_create_complex_tree_test() {
  binary_search_tree.to_tree([4, 2, 6, 1, 3, 5, 7])
  |> should.equal(Node(
    data: 4,
    left: Node(
      data: 2,
      left: Node(data: 1, left: Nil, right: Nil),
      right: Node(data: 3, left: Nil, right: Nil),
    ),
    right: Node(
      data: 6,
      left: Node(data: 5, left: Nil, right: Nil),
      right: Node(data: 7, left: Nil, right: Nil),
    ),
  ))
}

pub fn can_sort_data_can_sort_single_number_test() {
  binary_search_tree.sorted_data([2])
  |> should.equal([2])
}

pub fn can_sort_data_can_sort_if_second_number_is_smaller_than_first_test() {
  binary_search_tree.sorted_data([2, 1])
  |> should.equal([1, 2])
}

pub fn can_sort_data_can_sort_if_second_number_is_same_as_first_test() {
  binary_search_tree.sorted_data([2, 2])
  |> should.equal([2, 2])
}

pub fn can_sort_data_can_sort_if_second_number_is_greater_than_first_test() {
  binary_search_tree.sorted_data([2, 3])
  |> should.equal([2, 3])
}

pub fn can_sort_data_can_sort_complex_tree_test() {
  binary_search_tree.sorted_data([2, 1, 3, 6, 7, 5])
  |> should.equal([1, 2, 3, 5, 6, 7])
}
