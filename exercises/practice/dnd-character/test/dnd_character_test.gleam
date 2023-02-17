import gleam/iterator
import gleeunit
import gleeunit/should
import dnd_character

pub fn main() {
  gleeunit.main()
}

pub fn ability_modifier_ability_modifier_for_score_3_is__4_test() {
  dnd_character.modifier(3)
  |> should.equal(-4)
}

pub fn ability_modifier_ability_modifier_for_score_4_is__3_test() {
  dnd_character.modifier(4)
  |> should.equal(-3)
}

pub fn ability_modifier_ability_modifier_for_score_5_is__3_test() {
  dnd_character.modifier(5)
  |> should.equal(-3)
}

pub fn ability_modifier_ability_modifier_for_score_6_is__2_test() {
  dnd_character.modifier(6)
  |> should.equal(-2)
}

pub fn ability_modifier_ability_modifier_for_score_7_is__2_test() {
  dnd_character.modifier(7)
  |> should.equal(-2)
}

pub fn ability_modifier_ability_modifier_for_score_8_is__1_test() {
  dnd_character.modifier(8)
  |> should.equal(-1)
}

pub fn ability_modifier_ability_modifier_for_score_9_is__1_test() {
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

pub fn ability_modifier_ability_modifier_for_score_12_is__1_test() {
  dnd_character.modifier(12)
  |> should.equal(1)
}

pub fn ability_modifier_ability_modifier_for_score_13_is__1_test() {
  dnd_character.modifier(13)
  |> should.equal(1)
}

pub fn ability_modifier_ability_modifier_for_score_14_is__2_test() {
  dnd_character.modifier(14)
  |> should.equal(2)
}

pub fn ability_modifier_ability_modifier_for_score_15_is__2_test() {
  dnd_character.modifier(15)
  |> should.equal(2)
}

pub fn ability_modifier_ability_modifier_for_score_16_is__3_test() {
  dnd_character.modifier(16)
  |> should.equal(3)
}

pub fn ability_modifier_ability_modifier_for_score_17_is__3_test() {
  dnd_character.modifier(17)
  |> should.equal(3)
}

pub fn ability_modifier_ability_modifier_for_score_18_is__4_test() {
  dnd_character.modifier(18)
  |> should.equal(4)
}

pub fn random_ability_is_within_range_test() {
  iterator.repeatedly(dnd_character.ability)
  |> iterator.take(100)
  |> iterator.all(fn(ability) {
    assert True = ability >= 3
    assert True = ability <= 18
  })
}

pub fn random_character_charisma_is_within_range_test() {
  iterator.repeatedly(dnd_character.generate_character)
  |> iterator.take(100)
  |> iterator.all(fn(character) {
    assert True = character.charisma >= 3
    assert True = character.charisma <= 18
  })
}

pub fn random_character_constitution_is_within_range_test() {
  iterator.repeatedly(dnd_character.generate_character)
  |> iterator.take(100)
  |> iterator.all(fn(character) {
    assert True = character.constitution >= 3
    assert True = character.constitution <= 18
  })
}

pub fn random_character_dexterity_is_within_range_test() {
  iterator.repeatedly(dnd_character.generate_character)
  |> iterator.take(100)
  |> iterator.all(fn(character) {
    assert True = character.dexterity >= 3
    assert True = character.dexterity <= 18
  })
}

pub fn random_character_intelligence_is_within_range_test() {
  iterator.repeatedly(dnd_character.generate_character)
  |> iterator.take(100)
  |> iterator.all(fn(character) {
    assert True = character.intelligence >= 3
    assert True = character.intelligence <= 18
  })
}

pub fn random_character_strength_is_within_range_test() {
  iterator.repeatedly(dnd_character.generate_character)
  |> iterator.take(100)
  |> iterator.all(fn(character) {
    assert True = character.strength >= 3
    assert True = character.strength <= 18
  })
}

pub fn random_character_wisdom_is_within_range_test() {
  iterator.repeatedly(dnd_character.generate_character)
  |> iterator.take(100)
  |> iterator.all(fn(character) {
    assert True = character.wisdom >= 3
    assert True = character.wisdom <= 18
  })
}

pub fn random_character_hitpoints_is_calculated_correctly_test() {
  iterator.repeatedly(dnd_character.generate_character)
  |> iterator.take(100)
  |> iterator.all(fn(character) {
    assert True =
      10 + dnd_character.modifier(character.constitution) == character.hitpoints
  })
}
