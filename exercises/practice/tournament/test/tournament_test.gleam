import exercism/should
import exercism/test_runner
import tournament

pub fn main() {
  test_runner.main()
}

pub fn just_the_header_if_no_input_test() {
  tournament.tally("")
  |> should.equal("Team                           | MP |  W |  D |  L |  P")
}

pub fn a_win_is_three_points_a_loss_is_zero_points_test() {
  tournament.tally("Allegoric Alaskans;Blithering Badgers;win")
  |> should.equal(
    "Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  1 |  1 |  0 |  0 |  3
Blithering Badgers             |  1 |  0 |  0 |  1 |  0",
  )
}

pub fn a_win_can_also_be_expressed_as_a_loss_test() {
  tournament.tally("Blithering Badgers;Allegoric Alaskans;loss")
  |> should.equal(
    "Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  1 |  1 |  0 |  0 |  3
Blithering Badgers             |  1 |  0 |  0 |  1 |  0",
  )
}

pub fn a_different_team_can_win_test() {
  tournament.tally("Blithering Badgers;Allegoric Alaskans;win")
  |> should.equal(
    "Team                           | MP |  W |  D |  L |  P
Blithering Badgers             |  1 |  1 |  0 |  0 |  3
Allegoric Alaskans             |  1 |  0 |  0 |  1 |  0",
  )
}

pub fn a_draw_is_one_point_each_test() {
  tournament.tally("Allegoric Alaskans;Blithering Badgers;draw")
  |> should.equal(
    "Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  1 |  0 |  1 |  0 |  1
Blithering Badgers             |  1 |  0 |  1 |  0 |  1",
  )
}

pub fn there_can_be_more_than_one_match_test() {
  tournament.tally(
    "Allegoric Alaskans;Blithering Badgers;win
Allegoric Alaskans;Blithering Badgers;win",
  )
  |> should.equal(
    "Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  2 |  2 |  0 |  0 |  6
Blithering Badgers             |  2 |  0 |  0 |  2 |  0",
  )
}

pub fn there_can_be_more_than_one_winner_test() {
  tournament.tally(
    "Allegoric Alaskans;Blithering Badgers;loss
Allegoric Alaskans;Blithering Badgers;win",
  )
  |> should.equal(
    "Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  2 |  1 |  0 |  1 |  3
Blithering Badgers             |  2 |  1 |  0 |  1 |  3",
  )
}

pub fn there_can_be_more_than_two_teams_test() {
  tournament.tally(
    "Allegoric Alaskans;Blithering Badgers;win
Blithering Badgers;Courageous Californians;win
Courageous Californians;Allegoric Alaskans;loss",
  )
  |> should.equal(
    "Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  2 |  2 |  0 |  0 |  6
Blithering Badgers             |  2 |  1 |  0 |  1 |  3
Courageous Californians        |  2 |  0 |  0 |  2 |  0",
  )
}

pub fn typical_input_test() {
  tournament.tally(
    "Allegoric Alaskans;Blithering Badgers;win
Devastating Donkeys;Courageous Californians;draw
Devastating Donkeys;Allegoric Alaskans;win
Courageous Californians;Blithering Badgers;loss
Blithering Badgers;Devastating Donkeys;loss
Allegoric Alaskans;Courageous Californians;win",
  )
  |> should.equal(
    "Team                           | MP |  W |  D |  L |  P
Devastating Donkeys            |  3 |  2 |  1 |  0 |  7
Allegoric Alaskans             |  3 |  2 |  0 |  1 |  6
Blithering Badgers             |  3 |  1 |  0 |  2 |  3
Courageous Californians        |  3 |  0 |  1 |  2 |  1",
  )
}

pub fn incomplete_competition_not_all_pairs_have_played_test() {
  tournament.tally(
    "Allegoric Alaskans;Blithering Badgers;loss
Devastating Donkeys;Allegoric Alaskans;loss
Courageous Californians;Blithering Badgers;draw
Allegoric Alaskans;Courageous Californians;win",
  )
  |> should.equal(
    "Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  3 |  2 |  0 |  1 |  6
Blithering Badgers             |  2 |  1 |  1 |  0 |  4
Courageous Californians        |  2 |  0 |  1 |  1 |  1
Devastating Donkeys            |  1 |  0 |  0 |  1 |  0",
  )
}

pub fn ties_broken_alphabetically_test() {
  tournament.tally(
    "Courageous Californians;Devastating Donkeys;win
Allegoric Alaskans;Blithering Badgers;win
Devastating Donkeys;Allegoric Alaskans;loss
Courageous Californians;Blithering Badgers;win
Blithering Badgers;Devastating Donkeys;draw
Allegoric Alaskans;Courageous Californians;draw",
  )
  |> should.equal(
    "Team                           | MP |  W |  D |  L |  P
Allegoric Alaskans             |  3 |  2 |  1 |  0 |  7
Courageous Californians        |  3 |  2 |  1 |  0 |  7
Blithering Badgers             |  3 |  0 |  1 |  2 |  1
Devastating Donkeys            |  3 |  0 |  1 |  2 |  1",
  )
}

pub fn ensure_points_sorted_numerically_test() {
  tournament.tally(
    "Devastating Donkeys;Blithering Badgers;win
Devastating Donkeys;Blithering Badgers;win
Devastating Donkeys;Blithering Badgers;win
Devastating Donkeys;Blithering Badgers;win
Blithering Badgers;Devastating Donkeys;win",
  )
  |> should.equal(
    "Team                           | MP |  W |  D |  L |  P
Devastating Donkeys            |  5 |  4 |  0 |  1 | 12
Blithering Badgers             |  5 |  1 |  0 |  4 |  3",
  )
}
