import gleam/pair
import gleam/list

fn permutations(l: List(a)) -> List(List(a)) {
  case l {
    [] -> [[]]
    _ ->
      l
      |> list.index_map(fn(i_idx, i) {
        l
        |> list.index_fold(
          [],
          fn(acc, j, j_idx) {
            case i_idx == j_idx {
              True -> acc
              False -> [j, ..acc]
            }
          },
        )
        |> list.reverse
        |> permutations
        |> list.map(fn(permutation) { [i, ..permutation] })
      })
      |> list.flatten
  }
}

fn check(chain: List(#(Int, Int))) -> Bool {
  let checked_chain =
    list.fold_until(
      chain,
      [],
      fn(acc, tile) {
        case list.last(acc) {
          Ok(last_tile) ->
            case
              pair.first(last_tile) == pair.first(tile) || pair.first(last_tile) == pair.second(
                tile,
              ) || pair.second(last_tile) == pair.first(tile) || pair.second(
                last_tile,
              ) == pair.second(tile)
            {
              True ->
                list.Continue(
                  acc
                  |> list.reverse
                  |> list.prepend(tile)
                  |> list.reverse,
                )
              False -> list.Stop(acc)
            }
          Error(Nil) -> list.Continue([tile])
        }
      },
    )

  case list.length(checked_chain) == list.length(chain) {
    True -> {
      assert Ok(first_tile) = list.first(chain)
      assert Ok(last_tile) = list.last(chain)
      pair.first(first_tile) == pair.second(last_tile) || pair.first(first_tile) == pair.first(
        last_tile,
      )
    }
    False -> False
  }
}

pub fn arrange(chain: List(#(Int, Int))) -> List(List(#(Int, Int))) {
  case chain {
    [] -> []

    [tile] ->
      case pair.first(tile) == pair.second(tile) {
        True -> [chain]
        False -> []
      }

    _ ->
      chain
      |> permutations
      |> list.filter(check)
  }
}
