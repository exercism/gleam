import gleam/dict.{type Dict}
import gleam/list
import gleam/result

pub fn water_drinker() -> Result(String, Nil) {
  solve()
  |> result.map(fn(solution) { solution.water_drinker })
}

pub fn zebra_owner() -> Result(String, Nil) {
  solve()
  |> result.map(fn(solution) { solution.zebra_owner })
}

type Solution {
  Solution(water_drinker: String, zebra_owner: String)
}

fn solve() -> Result(Solution, Nil) {
  solve_for_colour(list.permutations(houses()), dict.new())
}

fn houses() {
  // clue 1
  [1, 2, 3, 4, 5]
}

fn right_of(a: Int, b: Int) -> Bool {
  a == b + 1
}

fn next_to(a: Int, b: Int) -> Bool {
  right_of(a, b) || right_of(b, a)
}

fn first() {
  1
}

fn middle() {
  3
}

fn solve_for_colour(
  permutations: List(List(Int)),
  parameters: Dict(String, Int),
) -> Result(Solution, Nil) {
  case permutations {
    [] -> Error(Nil)
    [perm, ..rest] -> {
      case perm {
        [red, green, ivory, yellow, blue] -> {
          // clue 6
          case right_of(green, ivory) {
            False -> solve_for_colour(rest, parameters)
            True -> {
              let parameters =
                parameters
                |> dict.insert("red", red)
                |> dict.insert("blue", blue)
                |> dict.insert("green", green)
                |> dict.insert("ivory", ivory)
                |> dict.insert("yellow", yellow)
              case
                solve_for_nationalities(list.permutations(houses()), parameters)
              {
                Error(Nil) -> solve_for_colour(rest, parameters)
                Ok(result) -> Ok(result)
              }
            }
          }
        }
        // unexpected permutation shape
        _ -> Error(Nil)
      }
    }
  }
}

fn solve_for_nationalities(
  permutations: List(List(Int)),
  parameters: Dict(String, Int),
) -> Result(Solution, Nil) {
  case permutations {
    [] -> Error(Nil)
    [perm, ..rest] -> {
      case perm {
        [english, spanish, ukranian, norwegian, japanese] -> {
          case dict.get(parameters, "red"), dict.get(parameters, "blue") {
            Ok(red), Ok(blue) -> {
              // clues: 2, 10, 15
              case
                english == red
                && norwegian == first()
                && next_to(norwegian, blue)
              {
                False -> solve_for_nationalities(rest, parameters)
                True -> {
                  let parameters =
                    parameters
                    |> dict.insert("english", english)
                    |> dict.insert("spanish", spanish)
                    |> dict.insert("ukranian", ukranian)
                    |> dict.insert("japanese", japanese)
                    |> dict.insert("norwegian", norwegian)
                  let nationalities =
                    dict.from_list([
                      #(english, "EnglishMan"),
                      #(spanish, "Spaniard"),
                      #(ukranian, "Ukranian"),
                      #(japanese, "Japanese"),
                      #(norwegian, "Norwegian"),
                    ])
                  case
                    solve_for_beverages(
                      list.permutations(houses()),
                      parameters,
                      nationalities,
                    )
                  {
                    Error(Nil) -> solve_for_nationalities(rest, parameters)
                    Ok(result) -> Ok(result)
                  }
                }
              }
            }
            // unexpected dictionary lookup errors
            _, _ -> Error(Nil)
          }
        }
        // unexpected permutation shape
        _ -> Error(Nil)
      }
    }
  }
}

fn solve_for_beverages(
  permutations: List(List(Int)),
  parameters: Dict(String, Int),
  nationalities: Dict(Int, String),
) -> Result(Solution, Nil) {
  case permutations {
    [] -> Error(Nil)
    [perm, ..rest] -> {
      case perm {
        [coffee, tea, milk, orange_juice, water] -> {
          case dict.get(parameters, "green"), dict.get(parameters, "ukranian") {
            Ok(green), Ok(ukranian) -> {
              // clues: 4, 5, 9
              case coffee == green && tea == ukranian && milk == middle() {
                False -> solve_for_beverages(rest, parameters, nationalities)
                True -> {
                  let parameters =
                    parameters
                    |> dict.insert("tea", tea)
                    |> dict.insert("milk", milk)
                    |> dict.insert("water", water)
                    |> dict.insert("coffee", coffee)
                    |> dict.insert("orange_juice", orange_juice)
                  case
                    solve_for_hobbies(
                      list.permutations(houses()),
                      parameters,
                      nationalities,
                    )
                  {
                    Error(Nil) ->
                      solve_for_beverages(rest, parameters, nationalities)
                    Ok(result) -> Ok(result)
                  }
                }
              }
            }
            // unexpected dictionary lookup errors
            _, _ -> Error(Nil)
          }
        }
        // unexpected permutation shape
        _ -> Error(Nil)
      }
    }
  }
}

fn solve_for_hobbies(
  permutations: List(List(Int)),
  parameters: Dict(String, Int),
  nationalities: Dict(Int, String),
) -> Result(Solution, Nil) {
  case permutations {
    [] -> Error(Nil)
    [perm, ..rest] -> {
      case perm {
        [dancing, painting, reading, football, chess] -> {
          case
            dict.get(parameters, "yellow"),
            dict.get(parameters, "japanese"),
            dict.get(parameters, "orange_juice")
          {
            Ok(yellow), Ok(japanese), Ok(orange_juice) -> {
              // clues: 8, 13, 14
              case
                painting == yellow
                && football == orange_juice
                && chess == japanese
              {
                False -> solve_for_hobbies(rest, parameters, nationalities)
                True -> {
                  let parameters =
                    parameters
                    |> dict.insert("dancing", dancing)
                    |> dict.insert("painting", painting)
                    |> dict.insert("football", football)
                    |> dict.insert("chess", chess)
                    |> dict.insert("reading", reading)
                  case
                    solve_for_pets(
                      list.permutations(houses()),
                      parameters,
                      nationalities,
                    )
                  {
                    Error(Nil) ->
                      solve_for_hobbies(rest, parameters, nationalities)
                    Ok(result) -> Ok(result)
                  }
                }
              }
            }
            // unexpected dictionary lookup errors
            _, _, _ -> Error(Nil)
          }
        }
        // unexpected permutation shape
        _ -> Error(Nil)
      }
    }
  }
}

fn solve_for_pets(
  permutations: List(List(Int)),
  parameters: Dict(String, Int),
  nationalities: Dict(Int, String),
) -> Result(Solution, Nil) {
  case permutations {
    [] -> Error(Nil)
    [perm, ..rest] -> {
      case perm {
        [dog, snails, fox, horse, zebra] -> {
          case
            dict.get(parameters, "dancing"),
            dict.get(parameters, "spanish"),
            dict.get(parameters, "painting"),
            dict.get(parameters, "reading")
          {
            Ok(dancing), Ok(spanish), Ok(painting), Ok(reading) -> {
              // clues: 3, 7, 11, 12
              case
                dog == spanish
                && snails == dancing
                && next_to(fox, reading)
                && next_to(horse, painting)
              {
                False -> solve_for_pets(rest, parameters, nationalities)
                True -> {
                  case dict.get(parameters, "water") {
                    Ok(water) -> {
                      case
                        dict.get(nationalities, water),
                        dict.get(nationalities, zebra)
                      {
                        Ok(water_owner), Ok(zebra_owner) ->
                          Ok(Solution(water_owner, zebra_owner))
                        _, _ -> Error(Nil)
                      }
                    }
                    // unexpected dictionary lookup errors
                    _ -> Error(Nil)
                  }
                }
              }
            }
            // unexpected dictionary lookup errors
            _, _, _, _ -> Error(Nil)
          }
        }
        // unexpected permutation shape
        _ -> Error(Nil)
      }
    }
  }
}
