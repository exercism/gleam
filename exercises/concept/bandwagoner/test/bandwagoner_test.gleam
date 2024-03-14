import bandwagoner.{Coach, Stats, Team}
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn create_coach_that_was_a_former_player_test() {
  bandwagoner.create_coach("Steve Kerr", True)
  |> should.equal(Coach(name: "Steve Kerr", former_player: True))
}

pub fn create_coach_that_wasnt_a_former_player_test() {
  bandwagoner.create_coach("Erik Spoelstra", False)
  |> should.equal(Coach(name: "Erik Spoelstra", former_player: False))
}

pub fn create_stats_for_winning_team_test() {
  bandwagoner.create_stats(55, 27)
  |> should.equal(Stats(wins: 55, losses: 27))
}

pub fn create_stats_for_losing_team_test() {
  bandwagoner.create_stats(39, 43)
  |> should.equal(Stats(wins: 39, losses: 43))
}

pub fn create_stats_for_all_time_season_record_test() {
  bandwagoner.create_stats(73, 9)
  |> should.equal(Stats(wins: 73, losses: 9))
}

pub fn create_60s_team_test() {
  let coach = bandwagoner.create_coach("Red Auerbach", False)
  let stats = bandwagoner.create_stats(58, 22)
  let team = bandwagoner.create_team("Boston Celtics", coach, stats)

  team
  |> should.equal(Team(
    name: "Boston Celtics",
    coach: Coach(name: "Red Auerbach", former_player: False),
    stats: Stats(wins: 58, losses: 22),
  ))
}

pub fn create_2010s_team_test() {
  let coach = bandwagoner.create_coach("Rick Carlisle", False)
  let stats = bandwagoner.create_stats(57, 25)
  let team = bandwagoner.create_team("Dallas Mavericks", coach, stats)

  team
  |> should.equal(Team(
    name: "Dallas Mavericks",
    coach: Coach(name: "Rick Carlisle", former_player: False),
    stats: Stats(wins: 57, losses: 25),
  ))
}

pub fn replace_coach_mid_season_test() {
  let old_coach = bandwagoner.create_coach("Willis Reed", True)
  let new_coach = bandwagoner.create_coach("Red Holzman", True)
  let stats = bandwagoner.create_stats(6, 8)
  let team = bandwagoner.create_team("New York Knicks", old_coach, stats)

  bandwagoner.replace_coach(team, new_coach)
  |> should.equal(Team(
    name: "New York Knicks",
    coach: Coach(name: "Red Holzman", former_player: True),
    stats: Stats(wins: 6, losses: 8),
  ))
}

pub fn replace_coach_after_season_test() {
  let old_coach = bandwagoner.create_coach("Rudy Tomjanovich", True)
  let new_coach = bandwagoner.create_coach("Jeff van Gundy", True)
  let stats = bandwagoner.create_stats(43, 39)
  let team = bandwagoner.create_team("Houston Rockets", old_coach, stats)

  bandwagoner.replace_coach(team, new_coach)
  |> should.equal(Team(
    name: "Houston Rockets",
    coach: Coach(name: "Jeff van Gundy", former_player: True),
    stats: Stats(wins: 43, losses: 39),
  ))
}

pub fn same_team_is_duplicate_test() {
  let coach = bandwagoner.create_coach("Pat Riley", True)
  let stats = bandwagoner.create_stats(57, 25)
  let team = bandwagoner.create_team("Los Angeles Lakers", coach, stats)

  let assert True = bandwagoner.is_same_team(team, team)
}

pub fn same_team_with_different_stats_is_not_a_duplicate_test() {
  let coach = bandwagoner.create_coach("Pat Riley", True)
  let stats = bandwagoner.create_stats(57, 25)
  let team = bandwagoner.create_team("Los Angeles Lakers", coach, stats)

  let new_stats = bandwagoner.create_stats(62, 20)
  let team_with_different_stats =
    bandwagoner.create_team("Los Angeles Lakers", coach, new_stats)

  let assert False = bandwagoner.is_same_team(team, team_with_different_stats)
}

pub fn same_team_with_different_coach_is_not_a_duplicate_test() {
  let coach = bandwagoner.create_coach("Pat Riley", True)
  let stats = bandwagoner.create_stats(33, 39)
  let team = bandwagoner.create_team("Los Angeles Lakers", coach, stats)

  let new_coach = bandwagoner.create_coach("John Kundla", True)
  let team_with_different_coach =
    bandwagoner.create_team("Los Angeles Lakers", new_coach, stats)

  let assert False = bandwagoner.is_same_team(team, team_with_different_coach)
}

pub fn different_team_with_same_coach_and_stats_test() {
  let stats = bandwagoner.create_stats(0, 0)
  let coach = bandwagoner.create_coach("Mike d'Antoni", True)

  let team = bandwagoner.create_team("Denver Nuggets", coach, stats)
  let other_team = bandwagoner.create_team("Phoenix Suns", coach, stats)

  let assert False = bandwagoner.is_same_team(team, other_team)
}

pub fn different_team_with_different_coach_and_stats_test() {
  let stats = bandwagoner.create_stats(42, 40)
  let coach = bandwagoner.create_coach("Dave Joerger", True)
  let team = bandwagoner.create_team("Memphis Grizzlies", coach, stats)

  let other_stats = bandwagoner.create_stats(63, 19)
  let other_coach = bandwagoner.create_coach("Larry Costello", True)
  let other_team =
    bandwagoner.create_team("Milwaukee Bucks", other_coach, other_stats)

  let assert False = bandwagoner.is_same_team(team, other_team)
}

pub fn root_for_team_with_favorite_coach_and_winning_stats_test() {
  let stats = bandwagoner.create_stats(60, 22)
  let coach = bandwagoner.create_coach("Gregg Popovich", False)
  let team = bandwagoner.create_team("San Antonio Spurs", coach, stats)

  let assert True = bandwagoner.root_for_team(team)
}

pub fn root_for_team_with_favorite_coach_and_losing_stats_test() {
  let stats = bandwagoner.create_stats(17, 47)
  let coach = bandwagoner.create_coach("Gregg Popovich", False)
  let team = bandwagoner.create_team("San Antonio Spurs", coach, stats)

  let assert True = bandwagoner.root_for_team(team)
}

pub fn root_for_team_with_coach_is_former_player_and_winning_stats_test() {
  let stats = bandwagoner.create_stats(49, 33)
  let coach = bandwagoner.create_coach("Jack Ramsay", True)
  let team = bandwagoner.create_team("Portland Trail Blazers", coach, stats)

  let assert True = bandwagoner.root_for_team(team)
}

pub fn root_for_team_with_coach_is_former_player_and_losing_stats_test() {
  let stats = bandwagoner.create_stats(0, 7)
  let coach = bandwagoner.create_coach("Jack Ramsay", True)
  let team = bandwagoner.create_team("Indiana Pacers", coach, stats)

  let assert True = bandwagoner.root_for_team(team)
}

pub fn root_for_favorite_team_and_winning_stats_test() {
  let stats = bandwagoner.create_stats(61, 21)
  let coach = bandwagoner.create_coach("Phil Jackson", True)
  let team = bandwagoner.create_team("Chicago Bulls", coach, stats)

  let assert True = bandwagoner.root_for_team(team)
}

pub fn root_for_favorite_team_and_losing_stats_test() {
  let stats = bandwagoner.create_stats(24, 58)
  let coach = bandwagoner.create_coach("Dick Motta", False)
  let team = bandwagoner.create_team("Chicago Bulls", coach, stats)

  let assert True = bandwagoner.root_for_team(team)
}

pub fn root_for_team_with_sixty_or_more_wins_and_former_player_coach_test() {
  let stats = bandwagoner.create_stats(65, 17)
  let coach = bandwagoner.create_coach("Billy Cunningham", True)
  let team = bandwagoner.create_team("Philadelphia 76'ers", coach, stats)

  let assert True = bandwagoner.root_for_team(team)
}

pub fn root_for_team_with_sixty_or_more_wins_and_non_former_player_coach_test() {
  let stats = bandwagoner.create_stats(60, 22)
  let coach = bandwagoner.create_coach("Mike Budenholzer", False)
  let team = bandwagoner.create_team("Milwaukee Bucks", coach, stats)

  let assert True = bandwagoner.root_for_team(team)
}

pub fn root_for_team_with_more_losses_than_wins_and_former_player_coach_test() {
  let stats = bandwagoner.create_stats(40, 42)
  let coach = bandwagoner.create_coach("Wes Unseld", True)
  let team = bandwagoner.create_team("Washington Bullets", coach, stats)

  let assert True = bandwagoner.root_for_team(team)
}

pub fn root_for_team_with_more_losses_than_wins_and_non_former_player_coach_test() {
  let stats = bandwagoner.create_stats(29, 43)
  let coach = bandwagoner.create_coach("Kenny Atkinson", False)
  let team = bandwagoner.create_team("Rochester Royals", coach, stats)

  let assert True = bandwagoner.root_for_team(team)
}

pub fn dont_root_for_team_not_matching_criteria_test() {
  let stats = bandwagoner.create_stats(51, 31)
  let coach = bandwagoner.create_coach("Frank Layden", False)
  let team = bandwagoner.create_team("Utah Jazz", coach, stats)

  let assert False = bandwagoner.root_for_team(team)
}
