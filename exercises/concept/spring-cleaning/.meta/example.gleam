import gleam/string

pub fn extract_error(problem: Result(a, b)) -> b {
  let assert Error(e) = problem
  e
}

pub fn remove_team_prefix(team: String) -> String {
  let assert "Team " <> rest = team
  rest
}

pub fn split_region_and_team(combined: String) -> #(String, String) {
  let assert [region, "Team " <> team] = string.split(combined, ",")
  #(region, team)
}
