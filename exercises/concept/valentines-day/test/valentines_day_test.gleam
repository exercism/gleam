import exercism/should
import exercism/test_runner
import valentines_day.{
  BoardGame, Chill, Crime, Horror, Korean, Maybe, Movie, No, Restaurant, Romance,
  Thriller, Turkish, Walk, Yes,
}

pub fn main() {
  test_runner.main()
}

pub fn rate_board_game_test() {
  valentines_day.rate_activity(BoardGame)
  |> should.equal(No)
}

pub fn rate_chilling_test() {
  valentines_day.rate_activity(Chill)
  |> should.equal(No)
}

pub fn rate_crime_movie_test() {
  valentines_day.rate_activity(Movie(Crime))
  |> should.equal(No)
}

pub fn rate_horror_movie_test() {
  valentines_day.rate_activity(Movie(Horror))
  |> should.equal(No)
}

pub fn rate_romance_movie_test() {
  valentines_day.rate_activity(Movie(Romance))
  |> should.equal(Yes)
}

pub fn rate_thriller_movie_test() {
  valentines_day.rate_activity(Movie(Thriller))
  |> should.equal(No)
}

pub fn rate_korean_restaurant_test() {
  valentines_day.rate_activity(Restaurant(Korean))
  |> should.equal(Yes)
}

pub fn rate_turkish_restaurant_test() {
  valentines_day.rate_activity(Restaurant(Turkish))
  |> should.equal(Maybe)
}

pub fn rate_walk_of_12_kilometer_test() {
  valentines_day.rate_activity(Walk(12))
  |> should.equal(Yes)
}

pub fn rate_walk_of_13_kilometers_test() {
  valentines_day.rate_activity(Walk(13))
  |> should.equal(Yes)
}

pub fn rate_walk_of_11_kilometers_test() {
  valentines_day.rate_activity(Walk(11))
  |> should.equal(Maybe)
}

pub fn rate_walk_of_7_kilometers_test() {
  valentines_day.rate_activity(Walk(7))
  |> should.equal(Maybe)
}

pub fn rate_walk_of_6_kilometers_test() {
  valentines_day.rate_activity(Walk(6))
  |> should.equal(No)
}
