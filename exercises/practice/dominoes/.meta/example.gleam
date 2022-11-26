import gleam/list
import gleam/queue

fn take_tile(chain, taker, rest) {
  case taker(chain) {
    Ok(#(tile, _)) -> rest(tile)
    Error(Nil) -> False
  }
}

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
    True -> {
      use first_tile <- take_tile(checked_chain, queue.pop_front)
      use last_tile <- take_tile(checked_chain, queue.pop_back)
      first_tile.0 == last_tile.0 || first_tile.0 == last_tile.1
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
