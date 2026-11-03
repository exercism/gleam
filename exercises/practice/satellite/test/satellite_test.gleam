import exercism/should
import exercism/test_runner
import satellite.{DifferentItems, DifferentLengths, Nil, Node, NonUniqueItems}

pub fn main() {
  test_runner.main()
}

pub fn empty_tree_test() {
  satellite.tree_from_traversals(inorder: [], preorder: [])
  |> should.equal(Ok(Nil))
}

pub fn tree_with_one_item_test() {
  satellite.tree_from_traversals(inorder: ["a"], preorder: ["a"])
  |> should.equal(
    Ok(Node(left: Nil, right: Nil, value: "a")),
  )
}

pub fn tree_with_many_items_test() {
  satellite.tree_from_traversals(inorder: ["i", "a", "f", "x", "right"], preorder: [
    "a", "i", "x", "f", "right",
  ])
  |> should.equal(
    Ok(Node(
      left: Node(left: Nil, right: Nil, value: "i"),
      right: Node(
        left: Node(
          left: Nil,
          right: Nil,
          value: "f",
        ),
        right: Node(
          left: Nil,
          right: Nil,
          value: "right",
        ),
        value: "x",
      ),
      value: "a",
    )),
  )
}

pub fn reject_traversals_of_different_length_test() {
  satellite.tree_from_traversals(inorder: ["b", "a", "right"], preorder: ["a", "b"])
  |> should.equal(Error(DifferentLengths))
}

pub fn reject_inconsistent_traversals_of_same_length_test() {
  satellite.tree_from_traversals(inorder: ["a", "b", "c"], preorder: [
    "x", "y", "z",
  ])
  |> should.equal(Error(DifferentItems))
}

pub fn reject_traversals_with_repeated_items_test() {
  satellite.tree_from_traversals(inorder: ["b", "a", "a"], preorder: [
    "a", "b", "a",
  ])
  |> should.equal(Error(NonUniqueItems))
}

pub fn a_degenerate_binary_tree_test() {
  satellite.tree_from_traversals(inorder: ["d", "c", "b", "a"], preorder: [
    "a", "b", "c", "d",
  ])
  |> should.equal(
    Ok(Node(
      left: Node(
        left: Node(
          left: Node(
            left: Nil,
            right: Nil,
            value: "d",
          ),
          right: Nil,
          value: "c",
        ),
        right: Nil,
        value: "b",
      ),
      right: Nil,
      value: "a",
    )),
  )
}

pub fn another_degenerate_binary_tree_test() {
  satellite.tree_from_traversals(inorder: ["a", "b", "c", "d"], preorder: [
    "a", "b", "c", "d",
  ])
  |> should.equal(
    Ok(Node(
      left: Nil,
      right: Node(
        left: Nil,
        right: Node(
          left: Nil,
          right: Node(
            left: Nil,
            right: Nil,
            value: "d",
          ),
          value: "c",
        ),
        value: "b",
      ),
      value: "a",
    )),
  )
}

pub fn tree_with_many_more_items_test() {
  satellite.tree_from_traversals(
    inorder: ["g", "d", "h", "b", "a", "e", "c", "i", "f"],
    preorder: ["a", "b", "d", "g", "h", "c", "e", "f", "i"],
  )
  |> should.equal(
    Ok(Node(
      left: Node(
        left: Node(
          left: Node(
            left: Nil,
            right: Nil,
            value: "g",
          ),
          right: Node(
            left: Nil,
            right: Nil,
            value: "h",
          ),
          value: "d",
        ),
        right: Nil,
        value: "b",
      ),
      right: Node(
        left: Node(
          left: Nil,
          right: Nil,
          value: "e",
        ),
        right: Node(
          left: Node(
            left: Nil,
            right: Nil,
            value: "i",
          ),
          right: Nil,
          value: "f",
        ),
        value: "c",
      ),
      value: "a",
    )),
  )
}
