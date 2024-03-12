import exercism/should
import exercism/test_runner
import gleam/dict
import word_count

pub fn main() {
  test_runner.main()
}

pub fn count_one_word_test() {
  "word"
  |> word_count.count_words
  |> should.equal(dict.from_list([#("word", 1)]))
}

pub fn count_one_of_each_word_test() {
  "one of each"
  |> word_count.count_words
  |> should.equal(dict.from_list([#("one", 1), #("of", 1), #("each", 1)]))
}

pub fn multiple_occurrences_of_a_word_test() {
  "one fish two fish red fish blue fish"
  |> word_count.count_words
  |> should.equal(
    dict.from_list([
      #("one", 1),
      #("fish", 4),
      #("two", 1),
      #("red", 1),
      #("blue", 1),
    ]),
  )
}

pub fn handles_cramped_lists_test() {
  "one,two,three"
  |> word_count.count_words
  |> should.equal(dict.from_list([#("one", 1), #("two", 1), #("three", 1)]))
}

pub fn handles_expanded_lists_test() {
  "one,\ntwo,\nthree"
  |> word_count.count_words
  |> should.equal(dict.from_list([#("one", 1), #("two", 1), #("three", 1)]))
}

pub fn ignore_punctuation_test() {
  "car: carpet as java: javascript!!&@$%^&"
  |> word_count.count_words
  |> should.equal(
    dict.from_list([
      #("car", 1),
      #("carpet", 1),
      #("as", 1),
      #("java", 1),
      #("javascript", 1),
    ]),
  )
}

pub fn include_numbers_test() {
  "testing, 1, 2 testing"
  |> word_count.count_words
  |> should.equal(dict.from_list([#("testing", 2), #("1", 1), #("2", 1)]))
}

pub fn normalize_case_test() {
  "go Go GO Stop stop"
  |> word_count.count_words
  |> should.equal(dict.from_list([#("go", 3), #("stop", 2)]))
}

pub fn with_apostrophes_test() {
  "'First: don't laugh. Then: don't cry. You're getting it.'"
  |> word_count.count_words
  |> should.equal(
    dict.from_list([
      #("first", 1),
      #("don't", 2),
      #("laugh", 1),
      #("then", 1),
      #("cry", 1),
      #("you're", 1),
      #("getting", 1),
      #("it", 1),
    ]),
  )
}

pub fn with_quotations_test() {
  "Joe can't tell between 'large' and large."
  |> word_count.count_words
  |> should.equal(
    dict.from_list([
      #("joe", 1),
      #("can't", 1),
      #("tell", 1),
      #("between", 1),
      #("large", 2),
      #("and", 1),
    ]),
  )
}

pub fn substrings_from_the_beginning_test() {
  "Joe can't tell between app, apple and a."
  |> word_count.count_words
  |> should.equal(
    dict.from_list([
      #("joe", 1),
      #("can't", 1),
      #("tell", 1),
      #("between", 1),
      #("app", 1),
      #("apple", 1),
      #("and", 1),
      #("a", 1),
    ]),
  )
}

pub fn multiple_spaces_not_detected_as_a_word_test() {
  " multiple   whitespaces"
  |> word_count.count_words
  |> should.equal(dict.from_list([#("multiple", 1), #("whitespaces", 1)]))
}

pub fn alternating_word_separators_not_detected_as_a_word_test() {
  ",\n,one,\n ,two \n 'three'"
  |> word_count.count_words
  |> should.equal(dict.from_list([#("one", 1), #("two", 1), #("three", 1)]))
}

pub fn quotation_for_word_with_apostrophe_test() {
  "can, can't, 'can't'"
  |> word_count.count_words
  |> should.equal(dict.from_list([#("can", 1), #("can't", 2)]))
}
