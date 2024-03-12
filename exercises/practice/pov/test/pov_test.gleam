import exercism/should
import exercism/test_runner
import gleam/list
import gleam/result
import gleam/string
import pov.{type Tree, Tree}

pub fn main() {
  test_runner.main()
}

pub fn reroot_a_tree_so_that_its_root_is_the_specified_node_results_in_the_same_tree_if_the_input_tree_is_a_singleton_test() {
  pov.from_pov(Tree("x", []), "x")
  |> should.equal(Ok(Tree("x", [])))
}

pub fn reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_tree_with_a_parent_and_one_sibling_test() {
  pov.from_pov(
    Tree(label: "parent", children: [Tree("x", []), Tree("sibling", [])]),
    "x",
  )
  |> result.map(normalize)
  |> should.equal(
    Ok(
      Tree(label: "x", children: [
        Tree(label: "parent", children: [Tree("sibling", [])]),
      ]),
    ),
  )
}

pub fn reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_tree_with_a_parent_and_many_siblings_test() {
  pov.from_pov(
    Tree(label: "parent", children: [
      Tree("a", []),
      Tree("b", []),
      Tree("c", []),
      Tree("x", []),
    ]),
    "x",
  )
  |> result.map(normalize)
  |> should.equal(
    Ok(
      Tree(label: "x", children: [
        Tree(label: "parent", children: [
          Tree("a", []),
          Tree("b", []),
          Tree("c", []),
        ]),
      ]),
    ),
  )
}

pub fn reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_tree_with_new_root_deeply_nested_in_tree_test() {
  pov.from_pov(
    Tree(label: "level-0", children: [
      Tree(label: "level-1", children: [
        Tree(label: "level-2", children: [
          Tree(label: "level-3", children: [Tree("x", [])]),
        ]),
      ]),
    ]),
    "x",
  )
  |> result.map(normalize)
  |> should.equal(
    Ok(
      Tree(label: "x", children: [
        Tree(label: "level-3", children: [
          Tree(label: "level-2", children: [
            Tree(label: "level-1", children: [Tree("level-0", [])]),
          ]),
        ]),
      ]),
    ),
  )
}

pub fn reroot_a_tree_so_that_its_root_is_the_specified_node_moves_children_of_the_new_root_to_same_level_as_former_parent_test() {
  pov.from_pov(
    Tree(label: "parent", children: [
      Tree(label: "x", children: [Tree("kid-0", []), Tree("kid-1", [])]),
    ]),
    "x",
  )
  |> result.map(normalize)
  |> should.equal(
    Ok(
      Tree(label: "x", children: [
        Tree("kid-0", []),
        Tree("kid-1", []),
        Tree("parent", []),
      ]),
    ),
  )
}

pub fn reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_complex_tree_with_cousins_test() {
  pov.from_pov(
    Tree(label: "grandparent", children: [
      Tree(label: "parent", children: [
        Tree(label: "x", children: [Tree("kid-0", []), Tree("kid-1", [])]),
        Tree("sibling-0", []),
        Tree("sibling-1", []),
      ]),
      Tree(label: "uncle", children: [
        Tree("cousin-0", []),
        Tree("cousin-1", []),
      ]),
    ]),
    "x",
  )
  |> result.map(normalize)
  |> should.equal(
    Ok(
      Tree(label: "x", children: [
        Tree("kid-0", []),
        Tree("kid-1", []),
        Tree(label: "parent", children: [
          Tree(label: "grandparent", children: [
            Tree(label: "uncle", children: [
              Tree("cousin-0", []),
              Tree("cousin-1", []),
            ]),
          ]),
          Tree("sibling-0", []),
          Tree("sibling-1", []),
        ]),
      ]),
    ),
  )
}

pub fn reroot_a_tree_so_that_its_root_is_the_specified_node_errors_if_target_does_not_exist_in_a_singleton_tree_test() {
  pov.from_pov(Tree("x", []), "nonexistent")
  |> should.equal(Error(Nil))
}

pub fn reroot_a_tree_so_that_its_root_is_the_specified_node_errors_if_target_does_not_exist_in_a_large_tree_test() {
  pov.from_pov(
    Tree(label: "parent", children: [
      Tree(label: "x", children: [Tree("kid-0", []), Tree("kid-1", [])]),
      Tree("sibling-0", []),
      Tree("sibling-1", []),
    ]),
    "nonexistent",
  )
  |> should.equal(Error(Nil))
}

pub fn given_two_nodes_find_the_path_between_them_can_find_path_to_parent_test() {
  pov.path_to(
    tree: Tree(label: "parent", children: [Tree("x", []), Tree("sibling", [])]),
    from: "x",
    to: "parent",
  )
  |> should.equal(Ok(["x", "parent"]))
}

pub fn given_two_nodes_find_the_path_between_them_can_find_path_to_sibling_test() {
  pov.path_to(
    tree: Tree(label: "parent", children: [
      Tree("a", []),
      Tree("x", []),
      Tree("b", []),
      Tree("c", []),
    ]),
    from: "x",
    to: "b",
  )
  |> should.equal(Ok(["x", "parent", "b"]))
}

pub fn given_two_nodes_find_the_path_between_them_can_find_path_to_cousin_test() {
  pov.path_to(
    tree: Tree(label: "grandparent", children: [
      Tree(label: "parent", children: [
        Tree(label: "x", children: [Tree("kid-0", []), Tree("kid-1", [])]),
        Tree("sibling-0", []),
        Tree("sibling-1", []),
      ]),
      Tree(label: "uncle", children: [
        Tree("cousin-0", []),
        Tree("cousin-1", []),
      ]),
    ]),
    from: "x",
    to: "cousin-1",
  )
  |> should.equal(Ok(["x", "parent", "grandparent", "uncle", "cousin-1"]))
}

pub fn given_two_nodes_find_the_path_between_them_can_find_path_not_involving_root_test() {
  pov.path_to(
    tree: Tree(label: "grandparent", children: [
      Tree(label: "parent", children: [
        Tree("x", []),
        Tree("sibling-0", []),
        Tree("sibling-1", []),
      ]),
    ]),
    from: "x",
    to: "sibling-1",
  )
  |> should.equal(Ok(["x", "parent", "sibling-1"]))
}

pub fn given_two_nodes_find_the_path_between_them_can_find_path_from_nodes_other_than_x_test() {
  pov.path_to(
    tree: Tree(label: "parent", children: [
      Tree("a", []),
      Tree("x", []),
      Tree("b", []),
      Tree("c", []),
    ]),
    from: "a",
    to: "c",
  )
  |> should.equal(Ok(["a", "parent", "c"]))
}

pub fn given_two_nodes_find_the_path_between_them_errors_if_destination_does_not_exist_test() {
  pov.path_to(
    tree: Tree(label: "parent", children: [
      Tree(label: "x", children: [Tree("kid-0", []), Tree("kid-1", [])]),
      Tree("sibling-0", []),
      Tree("sibling-1", []),
    ]),
    from: "x",
    to: "nonexistent",
  )
  |> should.equal(Error(Nil))
}

pub fn given_two_nodes_find_the_path_between_them_errors_if_source_does_not_exist_test() {
  pov.path_to(
    tree: Tree(label: "parent", children: [
      Tree(label: "x", children: [Tree("kid-0", []), Tree("kid-1", [])]),
      Tree("sibling-0", []),
      Tree("sibling-1", []),
    ]),
    from: "nonexistent",
    to: "x",
  )
  |> should.equal(Error(Nil))
}

fn normalize(tree: Tree(String)) -> Tree(String) {
  let Tree(label, children) = tree
  let children =
    children
    |> list.map(normalize)
    |> list.sort(fn(a, b) { string.compare(a.label, b.label) })
  Tree(label, children)
}
