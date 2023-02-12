import gleam/list
import gleam/int

pub fn scores(high_scores: List(Int)) -> List(Int) {
  high_scores
}

pub fn latest(high_scores: List(Int)) -> Int {
  assert Ok(latest_high_score) = list.last(high_scores)
  latest_high_score
}

pub fn personal_best(high_scores: List(Int)) -> Int {
  assert Ok(best_high_score) =
    high_scores
    |> list.sort(by: int.compare)
    |> list.last()
  best_high_score
}

pub fn personal_top_three(high_scores: List(Int)) -> List(Int) {
  high_scores
  |> list.sort(by: int.compare)
  |> list.reverse()
  |> list.take(3)
}
