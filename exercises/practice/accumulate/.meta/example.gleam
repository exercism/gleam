pub fn accumulate(list: List(a), fun: fn(a) -> b) -> List(b) {
  case list {
    [] -> []
    [x, ..xs] -> [fun(x), ..accumulate(xs, fun)]
  }
}
