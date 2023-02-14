pub type NestedList(a) {
  Nil
  Value(a)
  NestedList(List(NestedList(a)))
}

pub fn flatten(nested_list: NestedList(a)) -> List(a) {
  todo
}
