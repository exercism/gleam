import gleam/list

pub type NestedList(a) {
  Nil
  Value(a)
  NestedList(List(NestedList(a)))
}

pub fn flatten(nested_list: NestedList(a)) -> List(a) {
  case nested_list {
    Nil -> []
    Value(a) -> [a]
    NestedList(list) -> list.flat_map(list, flatten)
  }
}
