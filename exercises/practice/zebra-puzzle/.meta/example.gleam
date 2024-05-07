import gleam/dict.{type Dict}
import gleam/list

pub type Solution {
  Solution(water_drinker: String, zebra_owner: String)
}

type Parameters =
  Dict(String, Int)

type Nationalities =
  Dict(Int, String)

pub fn solve() -> Result(Solution, Nil) {
  solve_for_colour(
    houses()
      |> list.permutations(),
    dict.new(),
  )
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
  parameters: Parameters,
) -> Result(Solution, Nil) {
  case permutations {
    [] -> Error(Nil)
    [perm, ..rest] -> {
      let assert [red, green, ivory, yellow, blue] = perm

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
            solve_for_nationalities(
              houses()
                |> list.permutations(),
              parameters,
            )
          {
            Error(Nil) -> solve_for_colour(rest, parameters)
            Ok(result) -> Ok(result)
          }
        }
      }
    }
  }
}

fn solve_for_nationalities(
  permutations: List(List(Int)),
  parameters: Parameters,
) -> Result(Solution, Nil) {
  case permutations {
    [] -> Error(Nil)
    [perm, ..rest] -> {
      let assert [english, spanish, ukranian, norwegian, japanese] = perm
      let assert Ok(red) = dict.get(parameters, "red")
      let assert Ok(blue) = dict.get(parameters, "blue")

      // clues: 2, 10, 15
      case english == red && norwegian == first() && next_to(norwegian, blue) {
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
              houses()
                |> list.permutations(),
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
  }
}

fn solve_for_beverages(
  permutations: List(List(Int)),
  parameters: Parameters,
  nationalities: Nationalities,
) -> Result(Solution, Nil) {
  case permutations {
    [] -> Error(Nil)
    [perm, ..rest] -> {
      let assert [coffee, tea, milk, orange_juice, water] = perm
      let assert Ok(green) = dict.get(parameters, "green")
      let assert Ok(ukranian) = dict.get(parameters, "ukranian")

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
            solve_for_cigarettes(
              houses()
                |> list.permutations(),
              parameters,
              nationalities,
            )
          {
            Error(Nil) -> solve_for_beverages(rest, parameters, nationalities)
            Ok(result) -> Ok(result)
          }
        }
      }
    }
  }
}

fn solve_for_cigarettes(
  permutations: List(List(Int)),
  parameters: Parameters,
  nationalities: Nationalities,
) -> Result(Solution, Nil) {
  case permutations {
    [] -> Error(Nil)
    [perm, ..rest] -> {
      let assert [old_gold, kools, chesterfields, lucky_strike, parliaments] =
        perm
      let assert Ok(yellow) = dict.get(parameters, "yellow")
      let assert Ok(japanese) = dict.get(parameters, "japanese")
      let assert Ok(orange_juice) = dict.get(parameters, "orange_juice")

      // clues: 8, 13, 14
      case
        kools == yellow
        && lucky_strike == orange_juice
        && parliaments == japanese
      {
        False -> solve_for_cigarettes(rest, parameters, nationalities)
        True -> {
          let parameters =
            parameters
            |> dict.insert("kools", kools)
            |> dict.insert("old_gold", old_gold)
            |> dict.insert("lucky_strike", lucky_strike)
            |> dict.insert("parliaments", parliaments)
            |> dict.insert("chesterfields", chesterfields)
          case
            solve_for_pets(
              houses()
                |> list.permutations(),
              parameters,
              nationalities,
            )
          {
            Error(Nil) -> solve_for_cigarettes(rest, parameters, nationalities)
            Ok(result) -> Ok(result)
          }
        }
      }
    }
  }
}

fn solve_for_pets(
  permutations: List(List(Int)),
  parameters: Parameters,
  nationalities: Nationalities,
) -> Result(Solution, Nil) {
  case permutations {
    [] -> Error(Nil)
    [perm, ..rest] -> {
      let assert [dog, snails, fox, horse, zebra] = perm
      let assert Ok(kools) = dict.get(parameters, "kools")
      let assert Ok(spanish) = dict.get(parameters, "spanish")
      let assert Ok(old_gold) = dict.get(parameters, "old_gold")
      let assert Ok(chesterfields) = dict.get(parameters, "chesterfields")

      // clues: 3, 7, 11, 12
      case
        dog == spanish
        && snails == old_gold
        && next_to(fox, chesterfields)
        && next_to(horse, kools)
      {
        False -> solve_for_pets(rest, parameters, nationalities)
        True -> {
          let assert Ok(water) = dict.get(parameters, "water")
          let assert Ok(water_owner) = dict.get(nationalities, water)
          let assert Ok(zebra_owner) = dict.get(nationalities, zebra)
          Ok(Solution(water_owner, zebra_owner))
        }
      }
    }
  }
}
