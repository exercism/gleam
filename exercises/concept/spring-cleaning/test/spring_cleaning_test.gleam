import exercism/should
import exercism/test_runner
import spring_cleaning

pub fn main() {
  test_runner.main()
}

pub fn extract_error_projector_would_not_connect_test() {
  Error("Projector would not connect")
  |> spring_cleaning.extract_error
  |> should.equal("Projector would not connect")
}

pub fn extract_error_status_code_test() {
  Error(114_454)
  |> spring_cleaning.extract_error
  |> should.equal(114_454)
}

pub fn remove_team_prefix_team_mega_test() {
  "Team Mega"
  |> spring_cleaning.remove_team_prefix
  |> should.equal("Mega")
}

pub fn remove_team_prefix_team_sync_test() {
  "Team Sync"
  |> spring_cleaning.remove_team_prefix
  |> should.equal("Sync")
}

pub fn split_region_and_team_london_four_test() {
  "London,Team 4"
  |> spring_cleaning.split_region_and_team
  |> should.equal(#("London", "4"))
}

pub fn split_region_and_team_north_devvo_test() {
  "North,Team Devvo"
  |> spring_cleaning.split_region_and_team
  |> should.equal(#("North", "Devvo"))
}
