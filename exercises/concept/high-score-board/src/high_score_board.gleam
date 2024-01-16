import gleam/dict.{type Dict}

pub type ScoreBoard =
  Dict(String, Int)

pub fn create_score_board() -> ScoreBoard {
  todo
}

pub fn add_player(
  score_board: ScoreBoard,
  player: String,
  score: Int,
) -> ScoreBoard {
  todo
}

pub fn remove_player(score_board: ScoreBoard, player: String) -> ScoreBoard {
  todo
}

pub fn update_score(
  score_board: ScoreBoard,
  player: String,
  points: Int,
) -> ScoreBoard {
  todo
}

pub fn apply_monday_bonus(score_board: ScoreBoard) -> ScoreBoard {
  todo
}
