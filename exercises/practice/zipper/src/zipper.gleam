pub type Tree(a) {
  Leaf
  Node(value: a, left: Tree(a), right: Tree(a))
}

pub opaque type Zipper(a) {
  Todo
}

pub fn to_zipper(tree: Tree(a)) -> Zipper(a) {
  todo
}

pub fn to_tree(zipper: Zipper(a)) -> Tree(a) {
  todo
}

pub fn value(zipper: Zipper(a)) -> Result(a, Nil) {
  todo
}

pub fn up(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  todo
}

pub fn left(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  todo
}

pub fn right(zipper: Zipper(a)) -> Result(Zipper(a), Nil) {
  todo
}

pub fn set_value(zipper: Zipper(a), value: a) -> Zipper(a) {
  todo
}

pub fn set_left(zipper: Zipper(a), tree: Tree(a)) -> Result(Zipper(a), Nil) {
  todo
}

pub fn set_right(zipper: Zipper(a), tree: Tree(a)) -> Result(Zipper(a), Nil) {
  todo
}
