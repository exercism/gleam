import acronym
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn basic_test() {
  acronym.abbreviate(phrase: "Portable Network Graphics")
  |> should.equal("PNG")
}

pub fn lowercase_words_test() {
  acronym.abbreviate(phrase: "Ruby on Rails")
  |> should.equal("ROR")
}

pub fn punctuation_test() {
  acronym.abbreviate(phrase: "First In, First Out")
  |> should.equal("FIFO")
}

pub fn all_caps_word_test() {
  acronym.abbreviate(phrase: "GNU Image Manipulation Program")
  |> should.equal("GIMP")
}

pub fn punctuation_without_whitespace_test() {
  acronym.abbreviate(phrase: "Complementary metal-oxide semiconductor")
  |> should.equal("CMOS")
}

pub fn very_long_abbreviation_test() {
  acronym.abbreviate(
    phrase: "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me",
  )
  |> should.equal("ROTFLSHTMDCOALM")
}

pub fn consecutive_delimiters_test() {
  acronym.abbreviate(phrase: "Something - I made up from thin air")
  |> should.equal("SIMUFTA")
}

pub fn apostrophes_test() {
  acronym.abbreviate(phrase: "Halley's Comet")
  |> should.equal("HC")
}

pub fn underscore_emphasis_test() {
  acronym.abbreviate(phrase: "The Road _Not_ Taken")
  |> should.equal("TRNT")
}
