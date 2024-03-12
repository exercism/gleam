import exercism/should
import exercism/test_runner
import house

pub fn main() {
  test_runner.main()
}

pub fn verse_one_the_house_that_jack_built_test() {
  house.recite(start_verse: 1, end_verse: 1)
  |> should.equal("This is the house that Jack built.")
}

pub fn verse_two_the_malt_that_lay_test() {
  house.recite(start_verse: 2, end_verse: 2)
  |> should.equal("This is the malt that lay in the house that Jack built.")
}

pub fn verse_three_the_rat_that_ate_test() {
  house.recite(start_verse: 3, end_verse: 3)
  |> should.equal(
    "This is the rat that ate the malt that lay in the house that Jack built.",
  )
}

pub fn verse_four_the_cat_that_killed_test() {
  house.recite(start_verse: 4, end_verse: 4)
  |> should.equal(
    "This is the cat that killed the rat that ate the malt that lay in the house that Jack built.",
  )
}

pub fn verse_five_the_dog_that_worried_test() {
  house.recite(start_verse: 5, end_verse: 5)
  |> should.equal(
    "This is the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
  )
}

pub fn verse_six_the_cow_with_the_crumpled_horn_test() {
  house.recite(start_verse: 6, end_verse: 6)
  |> should.equal(
    "This is the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
  )
}

pub fn verse_seven_the_maiden_all_forlorn_test() {
  house.recite(start_verse: 7, end_verse: 7)
  |> should.equal(
    "This is the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
  )
}

pub fn verse_eight_the_man_all_tattered_and_torn_test() {
  house.recite(start_verse: 8, end_verse: 8)
  |> should.equal(
    "This is the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
  )
}

pub fn verse_nine_the_priest_all_shaven_and_shorn_test() {
  house.recite(start_verse: 9, end_verse: 9)
  |> should.equal(
    "This is the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
  )
}

pub fn verse_10_the_rooster_that_crowed_in_the_morn_test() {
  house.recite(start_verse: 10, end_verse: 10)
  |> should.equal(
    "This is the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
  )
}

pub fn verse_11_the_farmer_sowing_his_corn_test() {
  house.recite(start_verse: 11, end_verse: 11)
  |> should.equal(
    "This is the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
  )
}

pub fn verse_12_the_horse_and_the_hound_and_the_horn_test() {
  house.recite(start_verse: 12, end_verse: 12)
  |> should.equal(
    "This is the horse and the hound and the horn that belonged to the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
  )
}

pub fn multiple_verses_test() {
  house.recite(start_verse: 4, end_verse: 8)
  |> should.equal(
    "This is the cat that killed the rat that ate the malt that lay in the house that Jack built.
This is the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.
This is the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.
This is the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.
This is the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
  )
}

pub fn full_rhyme_test() {
  house.recite(start_verse: 1, end_verse: 12)
  |> should.equal(
    "This is the house that Jack built.
This is the malt that lay in the house that Jack built.
This is the rat that ate the malt that lay in the house that Jack built.
This is the cat that killed the rat that ate the malt that lay in the house that Jack built.
This is the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.
This is the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.
This is the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.
This is the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.
This is the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.
This is the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.
This is the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.
This is the horse and the hound and the horn that belonged to the farmer sowing his corn that kept the rooster that crowed in the morn that woke the priest all shaven and shorn that married the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.",
  )
}
