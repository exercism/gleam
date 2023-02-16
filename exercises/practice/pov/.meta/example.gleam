import gleam/list
import gleam/result

pub type Tree(a) {
  Tree(label: a, children: List(Tree(a)))
}

type Zipper(a) {
  Zipper(focus: Tree(a), List(Crumb(a)))
}

type Crumb(a) {
  Crumb(left: List(Tree(a)), parent: a, right: List(Tree(a)))
}

pub fn from_pov(tree: Tree(a), from: a) -> Result(Tree(a), Nil) {
  tree
  |> to_zipper
  |> move_to(from)
  |> result.map(to_rerouted_tree)
}

pub fn path_to(
  tree tree: Tree(a),
  from from: a,
  to to: a,
) -> Result(List(a), Nil) {
  tree
  |> to_zipper
  |> move_to(from)
  |> result.map(to_rerouted_tree)
  |> result.map(to_zipper)
  |> result.then(move_to(_, to))
  |> result.map(path_to_tree)
}

fn to_zipper(tree: Tree(a)) -> Zipper(a) {
  Zipper(tree, [])
}

fn move_to(zipper: Zipper(a), target: a) -> Result(Zipper(a), Nil) {
  let Zipper(tree, _) = zipper
  case tree {
    Tree(a, _) if a == target -> Ok(zipper)
    _ ->
      zipper
      |> down
      |> result.then(move_to(_, target))
      |> result.lazy_or(fn() {
        zipper
        |> right
        |> result.then(move_to(_, target))
      })
      |> result.lazy_or(fn() {
        zipper
        |> up_until_right
        |> result.then(move_to(_, target))
      })
  }
}

fn down(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  let Zipper(Tree(a, children), crumbs) = zipper
  case children {
    [] -> Error(Nil)
    [child, ..siblings] -> Ok(Zipper(child, [Crumb([], a, siblings), ..crumbs]))
  }
}

fn right(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  let Zipper(tree, crumbs) = zipper
  case crumbs {
    [] -> Error(Nil)
    [Crumb(_, _, []), ..] -> Error(Nil)
    [Crumb(left, parent, [sibling, ..right]), ..rest] ->
      Ok(Zipper(sibling, [Crumb([tree, ..left], parent, right), ..rest]))
  }
}

fn up_until_right(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  let Zipper(tree, crumbs) = zipper
  case crumbs {
    [] -> Error(Nil)
    [Crumb(left_siblings, parent, right_siblings), ..rest] -> {
      let parent_tree =
        Tree(parent, [tree])
        |> add_children(left_siblings)
        |> add_children(right_siblings)

      let up = Zipper(parent_tree, rest)
      case right(up) {
        Ok(up_right) -> Ok(up_right)
        Error(Nil) -> up_until_right(up)
      }
    }
  }
}

fn to_rerouted_tree(zipper: Zipper(a)) -> Tree(a) {
  let Zipper(tree, crumbs) = zipper
  case crumbs {
    [] -> tree
    [Crumb(left, parent, right), ..rest] -> {
      let child =
        Tree(parent, [])
        |> Zipper(rest)
        |> to_rerouted_tree
        |> add_children(left)
        |> add_children(right)

      add_children(tree, [child])
    }
  }
}

fn add_children(tree: Tree(a), new_children: List(Tree(a))) -> Tree(a) {
  let Tree(a, children) = tree
  Tree(a, list.append(children, new_children))
}

fn path_to_tree(zipper: Zipper(a)) -> List(a) {
  let Zipper(tree, crumbs) = zipper
  let path = list.map(crumbs, fn(crumb) { crumb.parent })
  list.reverse([tree.label, ..path])
}
