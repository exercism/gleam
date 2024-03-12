import exercism/should
import exercism/test_runner
import zipper.{Leaf, Node}

pub fn main() {
  test_runner.main()
}

pub fn data_is_retained_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  tree
  |> zipper.to_zipper
  |> zipper.to_tree
  |> should.equal(tree)
}

pub fn left_right_and_value_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Ok(zipper) = zipper.left(zipper)
  let assert Ok(zipper) = zipper.right(zipper)
  let assert Ok(3) = zipper.value(zipper)
}

pub fn dead_end_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Ok(zipper) = zipper.left(zipper)
  let assert Ok(zipper) = zipper.left(zipper)
  let assert Error(Nil) = zipper.left(zipper)
  let assert Error(Nil) = zipper.right(zipper)
}

pub fn tree_from_deep_focus_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Ok(zipper) = zipper.left(zipper)
  let assert Ok(zipper) = zipper.right(zipper)
  zipper
  |> zipper.to_tree
  |> should.equal(tree)
}

pub fn traversing_up_from_top_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Error(Nil) = zipper.up(zipper)
}

pub fn left_right_and_up_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Ok(zipper) = zipper.left(zipper)
  let assert Ok(zipper) = zipper.up(zipper)
  let assert Ok(zipper) = zipper.right(zipper)
  let assert Ok(zipper) = zipper.up(zipper)
  let assert Ok(zipper) = zipper.left(zipper)
  let assert Ok(zipper) = zipper.right(zipper)
  let assert Ok(3) = zipper.value(zipper)
}

pub fn test_ability_to_descend_multiple_levels_and_return_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Ok(zipper) = zipper.left(zipper)
  let assert Ok(zipper) = zipper.right(zipper)
  let assert Ok(zipper) = zipper.up(zipper)
  let assert Ok(zipper) = zipper.up(zipper)
  let assert Ok(1) = zipper.value(zipper)
}

pub fn set_value_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Ok(zipper) = zipper.left(zipper)

  zipper
  |> zipper.set_value(5)
  |> zipper.to_tree
  |> should.equal(Node(
    left: Node(
      left: Leaf,
      right: Node(value: 3, left: Leaf, right: Leaf),
      value: 5,
    ),
    right: Node(value: 4, left: Leaf, right: Leaf),
    value: 1,
  ))
}

pub fn set_value_after_traversing_up_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Ok(zipper) = zipper.left(zipper)
  let assert Ok(zipper) = zipper.right(zipper)
  let assert Ok(zipper) = zipper.up(zipper)

  zipper
  |> zipper.set_value(5)
  |> zipper.to_tree
  |> should.equal(Node(
    value: 1,
    left: Node(
      left: Leaf,
      right: Node(value: 3, left: Leaf, right: Leaf),
      value: 5,
    ),
    right: Node(value: 4, left: Leaf, right: Leaf),
  ))
}

pub fn set_left_with_leaf_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Ok(zipper) = zipper.left(zipper)
  let assert Ok(zipper) =
    zipper.set_left(zipper, Node(value: 5, left: Leaf, right: Leaf))

  zipper
  |> zipper.to_tree
  |> should.equal(Node(
    value: 1,
    left: Node(
      value: 2,
      left: Node(value: 5, left: Leaf, right: Leaf),
      right: Node(value: 3, left: Leaf, right: Leaf),
    ),
    right: Node(value: 4, left: Leaf, right: Leaf),
  ))
}

pub fn set_right_with_null_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Ok(zipper) = zipper.left(zipper)
  let assert Ok(zipper) = zipper.set_right(zipper, Leaf)

  zipper
  |> zipper.to_tree
  |> should.equal(Node(
    value: 1,
    left: Node(value: 2, left: Leaf, right: Leaf),
    right: Node(value: 4, left: Leaf, right: Leaf),
  ))
}

pub fn set_right_with_subtree_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Ok(zipper) =
    zipper.set_right(
      zipper,
      Node(
        value: 6,
        left: Node(value: 7, left: Leaf, right: Leaf),
        right: Node(value: 8, left: Leaf, right: Leaf),
      ),
    )

  zipper
  |> zipper.to_tree
  |> should.equal(Node(
    value: 1,
    left: Node(
      value: 2,
      left: Leaf,
      right: Node(value: 3, left: Leaf, right: Leaf),
    ),
    right: Node(
      value: 6,
      left: Node(value: 7, left: Leaf, right: Leaf),
      right: Node(value: 8, left: Leaf, right: Leaf),
    ),
  ))
}

pub fn set_value_on_deep_focus_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Ok(zipper) = zipper.left(zipper)
  let assert Ok(zipper) = zipper.right(zipper)

  zipper
  |> zipper.set_value(5)
  |> zipper.to_tree
  |> should.equal(Node(
    value: 1,
    left: Node(
      value: 2,
      left: Leaf,
      right: Node(value: 5, left: Leaf, right: Leaf),
    ),
    right: Node(value: 4, left: Leaf, right: Leaf),
  ))
}

pub fn different_paths_to_same_zipper_test() {
  let tree =
    Node(
      value: 1,
      left: Node(
        value: 2,
        left: Leaf,
        right: Node(value: 3, left: Leaf, right: Leaf),
      ),
      right: Node(value: 4, left: Leaf, right: Leaf),
    )

  let zipper = zipper.to_zipper(tree)
  let assert Ok(zipper) = zipper.left(zipper)
  let assert Ok(zipper) = zipper.up(zipper)
  let assert Ok(zipper) = zipper.right(zipper)

  let other_zipper = zipper.to_zipper(tree)
  let assert Ok(other_zipper) = zipper.right(other_zipper)

  should.equal(zipper, other_zipper)
}
