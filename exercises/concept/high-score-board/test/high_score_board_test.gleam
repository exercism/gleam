import exercism/should
import exercism/test_runner
import gleam/dict
import high_score_board

pub fn main() {
  test_runner.main()
}

pub fn create_score_board_test() {
  high_score_board.create_score_board()
  |> should.equal(dict.from_list([#("The Best Ever", 1_000_000)]))
}

pub fn add_player_board_test() {
  [#("Amil Pastorius", 99_373), #("Min-seo Shin", 0)]
  |> dict.from_list
  |> high_score_board.add_player("Jessie Johnson", 1337)
  |> should.equal(
    dict.from_list([
      #("Amil Pastorius", 99_373),
      #("Min-seo Shin", 0),
      #("Jessie Johnson", 1337),
    ]),
  )
}

pub fn remove_player_test() {
  [#("Amil Pastorius", 99_373), #("Min-seo Shin", 0), #("Jesse Johnson", 1337)]
  |> dict.from_list
  |> high_score_board.remove_player("Jesse Johnson")
  |> should.equal(
    dict.from_list([#("Amil Pastorius", 99_373), #("Min-seo Shin", 0)]),
  )
}

pub fn remove_player_unknown_test() {
  let board =
    dict.from_list([
      #("Amil Pastorius", 99_373),
      #("Min-seo Shin", 0),
      #("Jesse Johnson", 1337),
    ])

  board
  |> high_score_board.remove_player("Bruno Santangelo")
  |> should.equal(board)
}

pub fn update_score_test() {
  [#("Amil Pastorius", 99_373), #("Min-seo Shin", 0), #("Jesse Johnson", 1337)]
  |> dict.from_list
  |> high_score_board.update_score("Min-seo Shin", 1999)
  |> high_score_board.update_score("Jesse Johnson", 1337)
  |> high_score_board.update_score("Unknown player", 1)
  |> should.equal(
    dict.from_list([
      #("Amil Pastorius", 99_373),
      #("Min-seo Shin", 1999),
      #("Jesse Johnson", 2674),
    ]),
  )
}

pub fn apply_monday_bonus_test() {
  [#("Amil Pastorius", 345), #("Min-seo Shin", 19), #("Jesse Johnson", 122)]
  |> dict.from_list
  |> high_score_board.apply_monday_bonus
  |> should.equal(
    dict.from_list([
      #("Amil Pastorius", 445),
      #("Min-seo Shin", 119),
      #("Jesse Johnson", 222),
    ]),
  )
}
