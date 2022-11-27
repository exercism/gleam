import gleam/list
import gleam/queue

fn check(chain: List(#(Int, Int))) -> Bool {
  let checked_chain =
    list.fold_until(
      chain,
      queue.new(),
      fn(acc: queue.Queue(#(Int, Int)), tile) {
        case queue.pop_back(acc) {
          Ok(#(last_tile, _)) ->
            case
              last_tile.0 == tile.0 || last_tile.0 == tile.1 || last_tile.1 == tile.0 || last_tile.1 == tile.1
            {
              True ->
                acc
                |> queue.push_back(tile)
                |> list.Continue
              False -> list.Stop(acc)
            }
          Error(Nil) ->
            acc
            |> queue.push_back(tile)
            |> list.Continue
        }
      },
    )

  case queue.length(checked_chain) == list.length(chain) {
    True ->
      case queue.pop_front(checked_chain) {
        Ok(#(first_tile, _)) ->
          case queue.pop_back(checked_chain) {
            Ok(#(last_tile, _)) ->
              first_tile.0 == last_tile.0 || first_tile.0 == last_tile.1
            Error(Nil) -> False
          }
        Error(Nil) -> False
      }

    False -> False
  }
}

pub fn arrange(chain: List(#(Int, Int))) -> List(List(#(Int, Int))) {
  case chain {
    [] -> []

    [tile] ->
      case tile.0 == tile.1 {
        True -> [chain]
        False -> []
      }

    _ ->
      chain
      |> list.permutations
      |> list.filter(check)
  }
}
