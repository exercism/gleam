import gleam/list

pub fn keep(list: List(t), predicate: fn(t) -> Bool) -> List(t) {
  filter(list, predicate, [])
}

pub fn discard(list: List(t), predicate: fn(t) -> Bool) -> List(t) {
  keep(list, fn(e) { !predicate(e) })
}

fn filter(list, predicate, output_list) {
  case list {
    [] -> list.reverse(output_list)

    [head, ..tail] ->
      case predicate(head) {
        True -> filter(tail, predicate, [head, ..output_list])
        False -> filter(tail, predicate, output_list)
      }
  }
}
