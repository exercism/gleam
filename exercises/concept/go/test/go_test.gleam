import exercism/should
import exercism/test_runner
import go.{type Game, Black, Game, White}

pub fn main() {
  test_runner.main()
}

fn identity(x: Game) -> Game {
  x
}

fn identity_rule(game: Game) -> Result(Game, String) {
  Ok(game)
}

fn error_rule(_game: Game, message: String) -> Result(Game, String) {
  Error(message)
}

fn new_game() -> Game {
  Game(0, 0, White, "")
}

fn ko_rule() -> String {
  "Cannot repeat a previously played board position"
}

fn liberty_rule() -> String {
  "Cannot place a stone with no liberties"
}

fn one_stone_per_point_rule() -> String {
  "You can't put a stone on top of another stone"
}

fn add_white_captured_stone(game: Game) -> Game {
  Game(..game, white_captured_stones: 1)
}

pub fn change_player_if_all_rules_pass_test() {
  new_game()
  |> go.apply_rules(identity_rule, identity, identity_rule, identity_rule)
  |> should.equal(change_player(new_game()))
}

pub fn retain_error_and_player_if_ko_rule_fails_test() {
  new_game()
  |> go.apply_rules(
    identity_rule,
    identity,
    identity_rule,
    error_rule(_, ko_rule()),
  )
  |> should.equal(Game(..new_game(), error: ko_rule()))
}

pub fn retain_error_and_player_if_liberty_rule_fails_test() {
  new_game()
  |> go.apply_rules(
    identity_rule,
    identity,
    error_rule(_, liberty_rule()),
    identity_rule,
  )
  |> should.equal(Game(..new_game(), error: liberty_rule()))
}

pub fn retain_error_and_player_if_one_stone_per_point_rule_fails_test() {
  new_game()
  |> go.apply_rules(
    error_rule(_, one_stone_per_point_rule()),
    identity,
    identity_rule,
    identity_rule,
  )
  |> should.equal(Game(..new_game(), error: one_stone_per_point_rule()))
}

pub fn retain_changes_from_capture_rule_and_change_player_test() {
  new_game()
  |> go.apply_rules(
    identity_rule,
    add_white_captured_stone,
    identity_rule,
    identity_rule,
  )
  |> should.equal(
    new_game()
    |> add_white_captured_stone
    |> change_player,
  )
}

pub fn discard_changes_from_capture_rule_if_subsequent_rule_fails_test() {
  new_game()
  |> go.apply_rules(
    identity_rule,
    add_white_captured_stone,
    identity_rule,
    error_rule(_, ko_rule()),
  )
  |> should.equal(Game(..new_game(), error: ko_rule()))
}

fn change_player(game: Game) -> Game {
  let new_player = case game.player {
    White -> Black
    Black -> White
  }
  Game(..game, player: new_player)
}
