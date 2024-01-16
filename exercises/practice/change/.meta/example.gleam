import gleam/result
import gleam/list
import gleam/int
import gleam/dict.{type Dict}

pub type Error {
  ImpossibleTarget
}

pub fn find_fewest_coins(
  coins: List(Int),
  target: Int,
) -> Result(List(Int), Error) {
  list.range(1, target)
  |> list.fold(from: dict.from_list([#(0, [])]), with: fn(
    fewest_coins_map,
    coin,
  ) {
    update_fewest_coins_map(coins, fewest_coins_map, coin)
  })
  |> dict.get(target)
  |> result.replace_error(ImpossibleTarget)
}

fn calculate_fewest_coins(
  coins: List(Int),
  fewest_coins_map: Dict(Int, List(Int)),
  target: Int,
) -> Result(List(Int), Nil) {
  coins
  |> list.filter(fn(coin) { coin <= target })
  |> list.filter_map(fn(coin) {
    dict.get(fewest_coins_map, target - coin)
    |> result.map(fn(change) { [coin, ..change] })
  })
  |> list.sort(fn(a, b) { int.compare(list.length(a), list.length(b)) })
  |> list.first()
}

fn update_fewest_coins_map(
  coins: List(Int),
  fewest_coins_map: Dict(Int, List(Int)),
  amount: Int,
) -> Dict(Int, List(Int)) {
  calculate_fewest_coins(coins, fewest_coins_map, amount)
  |> result.map(dict.insert(fewest_coins_map, amount, _))
  |> result.unwrap(fewest_coins_map)
}
