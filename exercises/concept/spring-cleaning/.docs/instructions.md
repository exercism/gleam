# Instructions

Mika the events organise has got a promotion at work, and as part of her new responsibilities she is in charge of the events database. She quickly noticed that a lot of the data is stored in ways that make it hard to work with, so you're teaming up to clean it up with a one-off script.

Because the script is only going to be run once, and you are confident about the structure of the data, you feel this is a good place to use Gleam's _let assertions_.

## 1. Extract errors

Each past event has a list of problems that occurred during the event. These have been stored as results, but they're always the `Error` variant.


Implement the `extract_error` function which takes a result that is expected to always be an `Error` and returns the contained error value.

```gleam
extract_error(Error("Not enough coffee"))
// -> "Not enough coffee"
```

## 2. Remove team prefix

Team names have been prefixed with the word "Team ", but this is not needed. Implement the `remove_team_prefix` function to remove the prefix from a team name.

```gleam
remove_team_prefix("Team Turtle")
// -> "Turtle"
```

## 3. Split region and team

Each event is run by a team, and each team belongs to a region. In the database they are stored in a single string, separated by a comma.

Implement the `split_region_and_team` function to split the string into a tuple of two strings, one for the region and one for the team. The prefix "Team " should be removed from the team name.

```gleam
split_region_and_team("East,Team Orange")
// -> #("East", "Orange")
```
