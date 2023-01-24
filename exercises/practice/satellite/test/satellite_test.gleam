import gleeunit
import gleeunit/should
import satellite.{Nil, Node}

pub fn main() {
  gleeunit.main()
}

pub fn empty_tree_test() {
  satellite.tree_from_traversals(inorder: [], preorder: [])
  |> should.equal(Ok(Nil))
}

pub fn tree_with_one_item_test() {
  satellite.tree_from_traversals(inorder: ["a"], preorder: ["a"])
  |> should.equal(Ok(Node(value: "a", left: Nil, right: Nil)))
}

pub fn tree_with_many_items_test() {
  satellite.tree_from_traversals(
    inorder: ["i", "a", "f", "x", "r"],
    preorder: ["a", "i", "x", "f", "r"],
  )
  |> should.equal(Ok(Node(
    value: "a",
    left: Node(value: "i", left: Nil, right: Nil),
    right: Node(
      value: "x",
      left: Node(value: "f", left: Nil, right: Nil),
      right: Node(value: "r", left: Nil, right: Nil),
    ),
  )))
}

pub fn reject_traversals_of_different_length_test() {
  satellite.tree_from_traversals(inorder: ["b", "a", "r"], preorder: ["a", "b"])
  |> should.equal(Error("traversals must have the same length"))
}

pub fn reject_inconsistent_traversals_of_same_length_test() {
  satellite.tree_from_traversals(
    inorder: ["a", "b", "c"],
    preorder: ["x", "y", "z"],
  )
  |> should.equal(Error("traversals must have the same elements"))
}

pub fn reject_traversals_with_repeated_items_test() {
  satellite.tree_from_traversals(
    inorder: ["b", "a", "a"],
    preorder: ["a", "b", "a"],
  )
  |> should.equal(Error("traversals must contain unique items"))
}
