import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}
import gleam/string

pub fn tally(input: String) -> String {
  case input {
    "" -> header
    _ ->
      input
      |> get_team_scores
      |> print_table
  }
}

type Result {
  Win
  Draw
  Loss
}

type Score {
  Score(wins: Int, draws: Int, losses: Int)
}

const header = "Team                           | MP |  W |  D |  L |  P"

fn get_team_scores(input: String) -> Dict(String, Score) {
  input
  |> string.split("\n")
  |> list.fold(from: dict.new(), with: fn(scores, match) {
    let assert [team_a, team_b, result] = string.split(match, ";")
    let #(result_a, result_b) = parse_results(result)

    scores
    |> dict.upsert(team_a, increase(_, result_a))
    |> dict.upsert(team_b, increase(_, result_b))
  })
}

fn parse_results(result: String) -> #(Result, Result) {
  case result {
    "win" -> #(Win, Loss)
    "loss" -> #(Loss, Win)
    _draw -> #(Draw, Draw)
  }
}

fn increase(score: Option(Score), result: Result) -> Score {
  case result, score {
    Win, None -> Score(1, 0, 0)
    Draw, None -> Score(0, 1, 0)
    Loss, None -> Score(0, 0, 1)
    Win, Some(Score(w, d, l)) -> Score(w + 1, d, l)
    Draw, Some(Score(w, d, l)) -> Score(w, d + 1, l)
    Loss, Some(Score(w, d, l)) -> Score(w, d, l + 1)
  }
}

fn print_table(scores: Dict(String, Score)) -> String {
  let rows =
    scores
    |> dict.to_list
    |> list.sort(by: points_then_names)
    |> list.map(format_row)

  string.join([header, ..rows], "\n")
}

fn points_then_names(a: #(String, Score), b: #(String, Score)) -> Order {
  let #(name_a, score_a) = a
  let #(name_b, score_b) = b

  case int.compare(points(score_a), points(score_b)) {
    Gt -> Lt
    Lt -> Gt
    Eq -> string.compare(name_a, name_b)
  }
}

fn points(score: Score) -> Int {
  let Score(wins: wins, draws: draws, ..) = score
  3 * wins + draws
}

fn format_row(tally: #(String, Score)) -> String {
  let #(team, Score(wins: wins, draws: draws, losses: losses)) = tally
  let matches = int.to_string(wins + draws + losses)
  let points = int.to_string(3 * wins + draws)
  let wins = int.to_string(wins)
  let draws = int.to_string(draws)
  let losses = int.to_string(losses)

  [
    string.pad_right(team, to: 30, with: " "),
    string.pad_left(matches, to: 2, with: " "),
    string.pad_left(wins, to: 2, with: " "),
    string.pad_left(draws, to: 2, with: " "),
    string.pad_left(losses, to: 2, with: " "),
    string.pad_left(points, to: 2, with: " "),
  ]
  |> string.join(" | ")
}
