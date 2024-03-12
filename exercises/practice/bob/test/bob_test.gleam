import bob
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn stating_something_test() {
  bob.hey("Tom-ay-to, tom-aaaah-to.")
  |> should.equal("Whatever.")
}

pub fn shouting_test() {
  bob.hey("WATCH OUT!")
  |> should.equal("Whoa, chill out!")
}

pub fn shouting_gibberish_test() {
  bob.hey("FCECDFCAAB")
  |> should.equal("Whoa, chill out!")
}

pub fn asking_a_question_test() {
  bob.hey("Does this cryogenic chamber make me look fat?")
  |> should.equal("Sure.")
}

pub fn asking_a_numeric_question_test() {
  bob.hey("You are, what, like 15?")
  |> should.equal("Sure.")
}

pub fn asking_gibberish_test() {
  bob.hey("fffbbcbeab?")
  |> should.equal("Sure.")
}

pub fn talking_forcefully_test() {
  bob.hey("Hi there!")
  |> should.equal("Whatever.")
}

pub fn using_acronyms_in_regular_speech_test() {
  bob.hey("It's OK if you don't want to go work for NASA.")
  |> should.equal("Whatever.")
}

pub fn forceful_question_test() {
  bob.hey("WHAT'S GOING ON?")
  |> should.equal("Calm down, I know what I'm doing!")
}

pub fn shouting_numbers_test() {
  bob.hey("1, 2, 3 GO!")
  |> should.equal("Whoa, chill out!")
}

pub fn no_letters_test() {
  bob.hey("1, 2, 3")
  |> should.equal("Whatever.")
}

pub fn question_with_no_letters_test() {
  bob.hey("4?")
  |> should.equal("Sure.")
}

pub fn shouting_with_special_characters_test() {
  bob.hey("ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!")
  |> should.equal("Whoa, chill out!")
}

pub fn shouting_with_no_exclamation_mark_test() {
  bob.hey("I HATE THE DENTIST")
  |> should.equal("Whoa, chill out!")
}

pub fn statement_containing_question_mark_test() {
  bob.hey("Ending with ? means a question.")
  |> should.equal("Whatever.")
}

pub fn non_letters_with_question_test() {
  bob.hey(":) ?")
  |> should.equal("Sure.")
}

pub fn prattling_on_test() {
  bob.hey("Wait! Hang on. Are you going to be OK?")
  |> should.equal("Sure.")
}

pub fn silence_test() {
  bob.hey("")
  |> should.equal("Fine. Be that way!")
}

pub fn prolonged_silence_test() {
  bob.hey("          ")
  |> should.equal("Fine. Be that way!")
}

pub fn alternate_silence_test() {
  bob.hey("\t\t\t\t\t\t\t\t\t\t")
  |> should.equal("Fine. Be that way!")
}

pub fn multiple_line_question_test() {
  bob.hey("\nDoes this cryogenic chamber make me look fat?\nNo.")
  |> should.equal("Whatever.")
}

pub fn starting_with_whitespace_test() {
  bob.hey("         hmmmmmmm...")
  |> should.equal("Whatever.")
}

pub fn ending_with_whitespace_test() {
  bob.hey("Okay if like my  spacebar  quite a bit?   ")
  |> should.equal("Sure.")
}

pub fn other_whitespace_test() {
  bob.hey("\n\r \t")
  |> should.equal("Fine. Be that way!")
}

pub fn non_question_ending_with_whitespace_test() {
  bob.hey("This is a statement ending with whitespace      ")
  |> should.equal("Whatever.")
}
