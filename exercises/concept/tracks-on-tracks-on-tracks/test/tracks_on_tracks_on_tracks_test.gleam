import exercism/should
import exercism/test_runner
import tracks_on_tracks_on_tracks

pub fn main() {
  test_runner.main()
}

pub fn new_list_test() {
  tracks_on_tracks_on_tracks.new_list()
  |> should.equal([])
}

pub fn existing_list_test() {
  tracks_on_tracks_on_tracks.existing_list()
  |> should.equal(["Gleam", "Go", "TypeScript"])
}

pub fn add_language_to_new_list_test() {
  tracks_on_tracks_on_tracks.add_language([], "Elixir")
  |> should.equal(["Elixir"])
}

pub fn add_language_to_existing_list_test() {
  tracks_on_tracks_on_tracks.add_language(["Gleam", "Go", "TypeScript"], "Lua")
  |> should.equal(["Lua", "Gleam", "Go", "TypeScript"])
}

pub fn add_language_to_custom_list_test() {
  tracks_on_tracks_on_tracks.add_language(["Scheme"], "Racket")
  |> should.equal(["Racket", "Scheme"])
}

pub fn count_languages_on_new_list_test() {
  tracks_on_tracks_on_tracks.count_languages(
    tracks_on_tracks_on_tracks.new_list(),
  )
  |> should.equal(0)
}

pub fn count_languages_on_existing_list_test() {
  tracks_on_tracks_on_tracks.count_languages(
    tracks_on_tracks_on_tracks.existing_list(),
  )
  |> should.equal(3)
}

pub fn count_languages_on_custom_list_test() {
  tracks_on_tracks_on_tracks.count_languages(["Python", "JavaScript"])
  |> should.equal(2)
}

pub fn reverse_order_of_new_list_test() {
  tracks_on_tracks_on_tracks.reverse_list(tracks_on_tracks_on_tracks.new_list())
  |> should.equal([])
}

pub fn reverse_order_of_existing_list_test() {
  tracks_on_tracks_on_tracks.reverse_list(
    tracks_on_tracks_on_tracks.existing_list(),
  )
  |> should.equal(["TypeScript", "Go", "Gleam"])
}

pub fn reverse_order_of_custom_list_test() {
  tracks_on_tracks_on_tracks.reverse_list(["Kotlin", "Java", "Scala", "Clojure"])
  |> should.equal(["Clojure", "Scala", "Java", "Kotlin"])
}

pub fn empty_list_is_not_exciting_test() {
  let assert False = tracks_on_tracks_on_tracks.exciting_list([])
}

pub fn singleton_list_with_gleam_is_exciting_test() {
  let assert True = tracks_on_tracks_on_tracks.exciting_list(["Gleam"])
}

pub fn singleton_list_without_gleam_is_not_exciting_test() {
  let assert False = tracks_on_tracks_on_tracks.exciting_list(["Go"])
}

pub fn two_item_list_with_gleam_as_first_item_is_exciting_test() {
  let assert True =
    tracks_on_tracks_on_tracks.exciting_list(["Gleam", "Clojure"])
}

pub fn two_item_list_with_gleam_as_second_item_is_exciting_test() {
  let assert True = tracks_on_tracks_on_tracks.exciting_list(["Raku", "Gleam"])
}

pub fn two_item_list_without_gleam_is_not_exciting_test() {
  let assert False = tracks_on_tracks_on_tracks.exciting_list(["Python", "Go"])
}

pub fn three_item_list_with_gleam_as_first_item_is_exciting_test() {
  let assert True =
    tracks_on_tracks_on_tracks.exciting_list(["Gleam", "Lisp", "Clojure"])
}

pub fn three_item_list_with_gleam_as_second_item_is_exciting_test() {
  let assert True =
    tracks_on_tracks_on_tracks.exciting_list(["Java", "Gleam", "C#"])
}

pub fn three_item_list_with_gleam_as_third_item_is_not_exciting_test() {
  let assert False =
    tracks_on_tracks_on_tracks.exciting_list(["Julia", "Assembly", "Gleam"])
}

pub fn four_item_list_with_gleam_as_first_item_is_exciting_test() {
  let assert True =
    tracks_on_tracks_on_tracks.exciting_list(["Gleam", "C", "C++", "C#"])
}

pub fn four_item_list_with_gleam_as_second_item_is_not_exciting_test() {
  let assert False =
    tracks_on_tracks_on_tracks.exciting_list(["Elm", "Gleam", "C#", "Scheme"])
}

pub fn four_item_list_with_gleam_as_third_item_is_not_exciting_test() {
  let assert False =
    tracks_on_tracks_on_tracks.exciting_list(["Delphi", "D", "Gleam", "Prolog"])
}

pub fn four_item_list_with_gleam_as_fourth_item_is_not_exciting_test() {
  let assert False =
    tracks_on_tracks_on_tracks.exciting_list([
      "Julia", "Assembly", "Crystal", "Gleam",
    ])
}
