import gleam/list

pub type Tree {
  Nil
  Node(data: Int, left: Tree, right: Tree)
}

pub fn to_tree(data: List(Int)) -> Tree {
  list.fold(over: data, from: Nil, with: insert)
}

fn insert(tree: Tree, data: Int) -> Tree {
  case tree {
    Nil -> Node(data, Nil, Nil)
    Node(x, left, right) if data <= x -> Node(x, insert(left, data), right)
    Node(x, left, right) -> Node(x, left, insert(right, data))
  }
}

pub fn sorted_data(data: List(Int)) -> List(Int) {
  data
  |> to_tree
  |> to_sorted_data
}

fn to_sorted_data(tree: Tree) -> List(Int) {
  case tree {
    Nil -> []
    Node(x, left, right) ->
      list.flatten([to_sorted_data(left), [x], to_sorted_data(right)])
  }
}
