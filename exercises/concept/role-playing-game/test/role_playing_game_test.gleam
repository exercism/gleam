import exercism/should
import exercism/test_runner
import gleam/option.{None, Some}
import role_playing_game.{Player}

pub fn main() {
  test_runner.main()
}

pub fn introduce_someone_with_their_name_test() {
  Player(name: Some("Gandalf"), level: 1, health: 42, mana: None)
  |> role_playing_game.introduce
  |> should.equal("Gandalf")
}

pub fn introducing_an_unidentified_player_should_return_mighty_magician_test() {
  Player(name: None, level: 1, health: 42, mana: None)
  |> role_playing_game.introduce
  |> should.equal("Mighty Magician")
}

pub fn revive_a_player_that_is_alive_should_return_none_test() {
  Player(name: None, level: 12, health: 42, mana: Some(7))
  |> role_playing_game.revive
  |> should.equal(None)
}

pub fn reviving_a_low_level_player_resets_its_health_to_100_test() {
  Player(name: None, level: 3, health: 0, mana: None)
  |> role_playing_game.revive
  |> should.equal(Some(Player(name: None, level: 3, health: 100, mana: None)))
}

pub fn reviving_a_high_level_player_resets_both_its_health_and_mana_test() {
  Player(name: None, level: 10, health: 0, mana: Some(14))
  |> role_playing_game.revive
  |> should.equal(
    Some(Player(name: None, level: 10, health: 100, mana: Some(100))),
  )
}

pub fn cast_spell_causes_damage_of_double_the_mana_test() {
  Player(name: None, level: 10, health: 69, mana: Some(20))
  |> role_playing_game.cast_spell(9)
  |> should.equal(#(
    Player(name: None, level: 10, health: 69, mana: Some(11)),
    18,
  ))
}

pub fn casting_a_spell_with_insufficient_mana_does_none_test() {
  Player(name: None, level: 10, health: 69, mana: Some(20))
  |> role_playing_game.cast_spell(39)
  |> should.equal(#(
    Player(name: None, level: 10, health: 69, mana: Some(20)),
    0,
  ))
}

pub fn casting_a_spell_without_a_mana_pool_decreases_the_players_health_test() {
  Player(name: None, level: 5, health: 58, mana: None)
  |> role_playing_game.cast_spell(7)
  |> should.equal(#(Player(name: None, level: 5, health: 51, mana: None), 0))
}

pub fn a_players_health_cannot_go_below_0_test() {
  Player(name: None, level: 5, health: 6, mana: None)
  |> role_playing_game.cast_spell(12)
  |> should.equal(#(Player(name: None, level: 5, health: 0, mana: None), 0))
}
