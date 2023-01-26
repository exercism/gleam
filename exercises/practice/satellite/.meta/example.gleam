import gleam/list
import gleam/set

pub type Tree(a) {
  Nil
  Node(value: a, left: Tree(a), right: Tree(a))
}

pub fn tree_from_traversals(
  inorder inorder: List(a),
  preorder preorder: List(a),
) -> Result(Tree(a), String) {
  let inorder_set = set.from_list(inorder)
  let preorder_set = set.from_list(preorder)

  let inorder_length = list.length(inorder)
  let preorder_length = list.length(preorder)

  let different_length = inorder_length != preorder_length
  let different_elements = inorder_set != preorder_set
  let duplicate_elements = inorder_length != set.size(inorder_set)

  case Nil {
    _ if different_length -> Error("traversals must have the same length")
    _ if different_elements -> Error("traversals must have the same elements")
    _ if duplicate_elements -> Error("traversals must contain unique items")
    _ -> Ok(tree_from_traversals_safe(inorder, preorder))
  }
}

fn tree_from_traversals_safe(inorder: List(a), preorder: List(a)) -> Tree(a) {
  case preorder {
    [] -> Nil
    [value, ..rest_preorder] -> {
      let #(left_inorder, [_value, ..right_inorder]) =
        list.split_while(inorder, fn(x) { x != value })
      let #(left_preorder, right_preorder) =
        list.split(rest_preorder, list.length(left_inorder))

      let left = tree_from_traversals_safe(left_inorder, left_preorder)
      let right = tree_from_traversals_safe(right_inorder, right_preorder)
      Node(value: value, left: left, right: right)
    }
  }
}
