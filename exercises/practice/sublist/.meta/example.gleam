import gleam/int
import gleam/list
import gleam/function
import gleam/order.{Eq, Gt, Lt}

pub type Comparison {
  Equal
  Unequal
  Sublist
  Superlist
}

pub fn sublist(compare list_a: List(a), to list_b: List(a)) -> Comparison {
  let length_a = list.length(list_a)
  let length_b = list.length(list_b)

  case int.compare(length_a, length_b) {
    Eq ->
      case list_a == list_b {
        True -> Equal
        False -> Unequal
      }
    Gt -> uneven_length_sublist(list_b, list_a, flip_sub_super)
    Lt -> uneven_length_sublist(list_a, list_b, function.identity)
  }
}

fn flip_sub_super(comparison: Comparison) -> Comparison {
  case comparison {
    Sublist -> Superlist
    Superlist -> Sublist
    _ -> comparison
  }
}

fn uneven_length_sublist(
  smaller: List(a),
  larger: List(a),
  maybe_flip: fn(Comparison) -> Comparison,
) -> Comparison {
  case smaller, larger {
    [], _ -> maybe_flip(Sublist)
    _, [_, ..rest_larger] -> {
      let is_sublist =
        list.zip(smaller, larger)
        |> list.all(fn(pair) { pair.0 == pair.1 })

      case is_sublist {
        True -> maybe_flip(Sublist)
        False -> uneven_length_sublist(smaller, rest_larger, maybe_flip)
      }
    }
    _, [] -> Unequal
  }
}
