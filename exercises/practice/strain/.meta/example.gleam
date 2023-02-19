import gleam/list

pub fn keep(list: List(t), predicate: fn(t) -> Bool) -> List(t) {
  strain(list, predicate, [])
}

pub fn discard(list: List(t), predicate: fn(t) -> Bool) -> List(t) {
  strain(list, fn(e) { !predicate(e) }, [])
}

fn strain(list, predicate, output_list) {
  case list {
    [] -> list.reverse(output_list)

    [head, ..tail] ->
      case predicate(head) {
        True -> strain(tail, predicate, [head, ..output_list])
        False -> strain(tail, predicate, output_list)
      }
  }
}
