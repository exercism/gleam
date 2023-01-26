pub type Tree(a) {
  Leaf
  Node(value: a, left: Tree(a), right: Tree(a))
}

pub opaque type Zipper(a) {
  Zipper(focus: Tree(a), crumbs: List(#(Direction, a, Tree(a))))
}

type Direction {
  Left
  Right
}

pub fn to_zipper(tree: Tree(a)) -> Zipper(a) {
  Zipper(focus: tree, crumbs: [])
}

pub fn to_tree(zipper: Zipper(a)) -> Tree(a) {
  case up(zipper) {
    Ok(zipper) -> to_tree(zipper)
    Error(Nil) -> zipper.focus
  }
}

pub fn value(zipper: Zipper(a)) -> Result(a, Nil) {
  case zipper {
    Zipper(focus: Leaf, ..) -> Error(Nil)
    Zipper(focus: Node(value: value, ..), ..) -> Ok(value)
  }
}

pub fn up(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  case zipper {
    Zipper(crumbs: [], ..) -> Error(Nil)
    Zipper(focus: left, crumbs: [#(Left, value, right), ..crumbs]) ->
      Ok(Zipper(focus: Node(value, left, right), crumbs: crumbs))
    Zipper(focus: right, crumbs: [#(Right, value, left), ..crumbs]) ->
      Ok(Zipper(focus: Node(value, left, right), crumbs: crumbs))
  }
}

pub fn left(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  case zipper {
    Zipper(focus: Leaf, ..) -> Error(Nil)
    Zipper(focus: Node(value, left, right), crumbs: crumbs) ->
      Ok(Zipper(focus: left, crumbs: [#(Left, value, right), ..crumbs]))
  }
}

pub fn right(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  case zipper {
    Zipper(focus: Leaf, ..) -> Error(Nil)
    Zipper(focus: Node(value, left, right), crumbs: crumbs) ->
      Ok(Zipper(focus: right, crumbs: [#(Right, value, left), ..crumbs]))
  }
}

pub fn set_value(zipper: Zipper(a), value: a) -> Zipper(a) {
  case zipper {
    Zipper(focus: Leaf, ..) -> Zipper(..zipper, focus: Node(value, Leaf, Leaf))
    Zipper(focus: Node(_, left, right), ..) ->
      Zipper(..zipper, focus: Node(value, left, right))
  }
}

pub fn set_left(zipper: Zipper(a), tree: Tree(a)) -> Result(Zipper(a), Nil) {
  case zipper {
    Zipper(focus: Leaf, ..) -> Error(Nil)
    Zipper(focus: Node(value, _, right), ..) ->
      Ok(Zipper(..zipper, focus: Node(value, tree, right)))
  }
}

pub fn set_right(zipper: Zipper(a), tree: Tree(a)) -> Result(Zipper(a), Nil) {
  case zipper {
    Zipper(focus: Leaf, ..) -> Error(Nil)
    Zipper(focus: Node(value, left, _), ..) ->
      Ok(Zipper(..zipper, focus: Node(value, left, tree)))
  }
}
