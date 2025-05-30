import gleam/deque
import gleam/list

pub fn can_chain(chain: List(#(Int, Int))) -> Bool {
  case chain {
    [] -> True

    [tile] -> tile.0 == tile.1

    _ -> {
      let no_solution =
        chain
        |> list.permutations
        |> list.filter(check)
        |> list.is_empty

      !no_solution
    }
  }
}

fn check(chain: List(#(Int, Int))) -> Bool {
  let checked_chain =
    list.fold_until(chain, deque.new(), fn(acc: deque.Deque(#(Int, Int)), tile) {
      case deque.pop_back(acc) {
        Ok(#(last_tile, _)) ->
          case
            last_tile.0 == tile.0
            || last_tile.0 == tile.1
            || last_tile.1 == tile.0
            || last_tile.1 == tile.1
          {
            True ->
              acc
              |> deque.push_back(tile)
              |> list.Continue
            False -> list.Stop(acc)
          }
        Error(Nil) ->
          acc
          |> deque.push_back(tile)
          |> list.Continue
      }
    })

  case deque.length(checked_chain) == list.length(chain) {
    True ->
      case deque.pop_front(checked_chain) {
        Ok(#(first_tile, _)) ->
          case deque.pop_back(checked_chain) {
            Ok(#(last_tile, _)) ->
              first_tile.0 == last_tile.0 || first_tile.0 == last_tile.1
            Error(Nil) -> False
          }
        Error(Nil) -> False
      }

    False -> False
  }
}
