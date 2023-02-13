import gleam/bitwise
import gleam/list

pub type Allergen {
  Eggs
  Peanuts
  Shellfish
  Strawberries
  Tomatoes
  Chocolate
  Pollen
  Cats
}

fn to_score(allergen: Allergen) -> Int {
  case allergen {
    Eggs -> 1
    Peanuts -> 2
    Shellfish -> 4
    Strawberries -> 8
    Tomatoes -> 16
    Chocolate -> 32
    Pollen -> 64
    Cats -> 128
  }
}

pub fn allergic_to(allergen: Allergen, score: Int) -> Bool {
  bitwise.and(to_score(allergen), score) != 0
}

pub fn list(score: Int) -> List(Allergen) {
  [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]
  |> list.filter(allergic_to(_, score))
}
