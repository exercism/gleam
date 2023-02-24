import gleam/map
import gleam/list
import gleeunit
import gleeunit/should
import parallel_letter_frequency

pub fn main() {
  gleeunit.main()
}

pub fn no_texts_test() {
  parallel_letter_frequency.calculate_frequencies([])
  |> should.equal(map.new())
}

pub fn one_text_with_one_letter_test() {
  parallel_letter_frequency.calculate_frequencies(["a"])
  |> should.equal(map.from_list([#("a", 1)]))
}

pub fn one_text_with_multiple_letters_test() {
  parallel_letter_frequency.calculate_frequencies(["bbcccd"])
  |> should.equal(map.from_list([#("b", 2), #("c", 3), #("d", 1)]))
}

pub fn two_texts_with_one_letter_test() {
  parallel_letter_frequency.calculate_frequencies(["e", "f"])
  |> should.equal(map.from_list([#("e", 1), #("f", 1)]))
}

pub fn two_texts_with_multiple_letters_test() {
  parallel_letter_frequency.calculate_frequencies(["ggh", "hhi"])
  |> should.equal(map.from_list([#("g", 2), #("h", 3), #("i", 1)]))
}

pub fn many_texts_result_test() {
  parallel_letter_frequency.calculate_frequencies(list.repeat("xyz", 1000))
  |> should.equal(map.from_list([#("x", 1000), #("y", 1000), #("z", 1000)]))
}

pub fn ignore_letter_casing_test() {
  parallel_letter_frequency.calculate_frequencies(["b", "B"])
  |> should.equal(map.from_list([#("b", 2)]))
}

pub fn ignore_whitespace_test() {
  parallel_letter_frequency.calculate_frequencies([" ", "\t", "\r\n"])
  |> should.equal(map.new())
}

pub fn ignore_punctuation_test() {
  parallel_letter_frequency.calculate_frequencies(["!", ";", ",", "."])
  |> should.equal(map.new())
}

pub fn ignore_numbers_test() {
  parallel_letter_frequency.calculate_frequencies(["1", "2", "3"])
  |> should.equal(map.new())
}

pub fn non_ascii_letters_test() {
  parallel_letter_frequency.calculate_frequencies(["本", "φ", "ほ", "ø"])
  |> should.equal(map.from_list([
    #("本", 1),
    #("φ", 1),
    #("ほ", 1),
    #("ø", 1),
  ]))
}

pub fn multiple_texts_with_many_different_characters_test() {
  let frequencies =
    parallel_letter_frequency.calculate_frequencies([
      ode_an_die_freude,
      wilhelmus,
      star_spangled_banner,
    ])

  should.equal(27, map.size(frequencies))
  should.equal(Ok(102), map.get(frequencies, "e"))
  should.equal(Ok(50), map.get(frequencies, "r"))
  should.equal(Ok(49), map.get(frequencies, "a"))
  should.equal(Ok(56), map.get(frequencies, "t"))
  should.equal(Ok(5), map.get(frequencies, "j"))
  should.equal(Ok(2), map.get(frequencies, "ü"))
  should.equal(Ok(2), map.get(frequencies, "ö"))
}

// European national anthem
const ode_an_die_freude = "Freude schöner Götterfunken
Tochter aus Elysium,
Wir betreten feuertrunken,
Himmlische, dein Heiligtum!
Deine Zauber binden wieder
Was die Mode streng geteilt;
Alle Menschen werden Brüder,
Wo dein sanfter Flügel weilt."

// Dutch national anthem
const wilhelmus = "Wilhelmus van Nassouwe
ben ik, van Duitsen bloed,
den vaderland getrouwe
blijf ik tot in den dood.
Een Prinse van Oranje
ben ik, vrij, onverveerd,
den Koning van Hispanje
heb ik altijd geëerd.
"

// American national anthem
const star_spangled_banner = "O say can you see by the dawn's early light,
What so proudly we hailed at the twilight's last gleaming,
Whose broad stripes and bright stars through the perilous fight,
O'er the ramparts we watched, were so gallantly streaming?
And the rockets' red glare, the bombs bursting in air,
Gave proof through the night that our flag was still there;
O say does that star-spangled banner yet wave,
O'er the land of the free and the home of the brave?
"
