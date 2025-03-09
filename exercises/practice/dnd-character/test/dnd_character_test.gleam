import dnd_character
import exercism/should
import exercism/test_runner
import gleam/yielder

pub fn main() {
  test_runner.main()
}

pub fn ability_modifier_ability_modifier_for_score_3_is_4_test() {
  dnd_character.modifier(3)
  |> should.equal(-4)
}

pub fn ability_modifier_ability_modifier_for_score_4_is_3_test() {
  dnd_character.modifier(4)
  |> should.equal(-3)
}

pub fn ability_modifier_ability_modifier_for_score_5_is_3_test() {
  dnd_character.modifier(5)
  |> should.equal(-3)
}

pub fn ability_modifier_ability_modifier_for_score_6_is_2_test() {
  dnd_character.modifier(6)
  |> should.equal(-2)
}

pub fn ability_modifier_ability_modifier_for_score_7_is_2_test() {
  dnd_character.modifier(7)
  |> should.equal(-2)
}

pub fn ability_modifier_ability_modifier_for_score_8_is_1_test() {
  dnd_character.modifier(8)
  |> should.equal(-1)
}

pub fn ability_modifier_ability_modifier_for_score_9_is_1_test() {
  dnd_character.modifier(9)
  |> should.equal(-1)
}

pub fn ability_modifier_ability_modifier_for_score_10_is_0_test() {
  dnd_character.modifier(10)
  |> should.equal(0)
}

pub fn ability_modifier_ability_modifier_for_score_11_is_0_test() {
  dnd_character.modifier(11)
  |> should.equal(0)
}

pub fn ability_modifier_ability_modifier_for_score_12_is_1_test() {
  dnd_character.modifier(12)
  |> should.equal(1)
}

pub fn ability_modifier_ability_modifier_for_score_13_is_1_test() {
  dnd_character.modifier(13)
  |> should.equal(1)
}

pub fn ability_modifier_ability_modifier_for_score_14_is_2_test() {
  dnd_character.modifier(14)
  |> should.equal(2)
}

pub fn ability_modifier_ability_modifier_for_score_15_is_2_test() {
  dnd_character.modifier(15)
  |> should.equal(2)
}

pub fn ability_modifier_ability_modifier_for_score_16_is_3_test() {
  dnd_character.modifier(16)
  |> should.equal(3)
}

pub fn ability_modifier_ability_modifier_for_score_17_is_3_test() {
  dnd_character.modifier(17)
  |> should.equal(3)
}

pub fn ability_modifier_ability_modifier_for_score_18_is_4_test() {
  dnd_character.modifier(18)
  |> should.equal(4)
}

pub fn random_ability_is_within_range_test() {
  yielder.repeatedly(dnd_character.ability)
  |> yielder.take(100)
  |> yielder.all(fn(ability) {
    let assert True = ability >= 3
    let assert True = ability <= 18
  })
}

pub fn random_character_charisma_is_within_range_test() {
  yielder.repeatedly(dnd_character.generate_character)
  |> yielder.take(100)
  |> yielder.all(fn(character) {
    let assert True = character.charisma >= 3
    let assert True = character.charisma <= 18
  })
}

pub fn random_character_constitution_is_within_range_test() {
  yielder.repeatedly(dnd_character.generate_character)
  |> yielder.take(100)
  |> yielder.all(fn(character) {
    let assert True = character.constitution >= 3
    let assert True = character.constitution <= 18
  })
}

pub fn random_character_dexterity_is_within_range_test() {
  yielder.repeatedly(dnd_character.generate_character)
  |> yielder.take(100)
  |> yielder.all(fn(character) {
    let assert True = character.dexterity >= 3
    let assert True = character.dexterity <= 18
  })
}

pub fn random_character_intelligence_is_within_range_test() {
  yielder.repeatedly(dnd_character.generate_character)
  |> yielder.take(100)
  |> yielder.all(fn(character) {
    let assert True = character.intelligence >= 3
    let assert True = character.intelligence <= 18
  })
}

pub fn random_character_strength_is_within_range_test() {
  yielder.repeatedly(dnd_character.generate_character)
  |> yielder.take(100)
  |> yielder.all(fn(character) {
    let assert True = character.strength >= 3
    let assert True = character.strength <= 18
  })
}

pub fn random_character_wisdom_is_within_range_test() {
  yielder.repeatedly(dnd_character.generate_character)
  |> yielder.take(100)
  |> yielder.all(fn(character) {
    let assert True = character.wisdom >= 3
    let assert True = character.wisdom <= 18
  })
}

pub fn random_character_hitpoints_is_calculated_correctly_test() {
  yielder.repeatedly(dnd_character.generate_character)
  |> yielder.take(100)
  |> yielder.all(fn(character) {
    let assert True =
      10 + dnd_character.modifier(character.constitution) == character.hitpoints
  })
}
