import gleam/float
import gleam/int
import gleam/list
import gleam/yielder

pub type Character {
  Character(
    charisma: Int,
    constitution: Int,
    dexterity: Int,
    hitpoints: Int,
    intelligence: Int,
    strength: Int,
    wisdom: Int,
  )
}

pub fn generate_character() -> Character {
  let constitution = ability()

  Character(
    charisma: ability(),
    constitution: constitution,
    dexterity: ability(),
    hitpoints: 10 + modifier(constitution),
    intelligence: ability(),
    strength: ability(),
    wisdom: ability(),
  )
}

pub fn modifier(score: Int) -> Int {
  { int.to_float(score) -. 10.0 } /. 2.0
  |> float.floor()
  |> float.round()
}

pub fn ability() -> Int {
  yielder.repeatedly(roll)
  |> yielder.take(4)
  |> yielder.to_list()
  |> list.sort(int.compare)
  |> list.drop(1)
  |> int.sum()
}

fn roll() -> Int {
  int.random(6) + 1
}
