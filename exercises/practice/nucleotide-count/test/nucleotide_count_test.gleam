import exercism/should
import exercism/test_runner
import gleam/dict
import nucleotide_count

pub fn main() {
  test_runner.main()
}

pub fn empty_strand_test() {
  let strand = ""
  let expected =
    [#("A", 0), #("C", 0), #("G", 0), #("T", 0)]
    |> dict.from_list
    |> Ok
  nucleotide_count.nucleotide_count(strand)
  |> should.equal(expected)
}

pub fn can_count_one_nucleotide_in_single_character_input_test() {
  let strand = "G"
  let expected =
    [#("A", 0), #("C", 0), #("G", 1), #("T", 0)]
    |> dict.from_list
    |> Ok
  nucleotide_count.nucleotide_count(strand)
  |> should.equal(expected)
}

pub fn strand_with_repeated_nucleotide_test() {
  let strand = "GGGGGGG"
  let expected =
    [#("A", 0), #("C", 0), #("G", 7), #("T", 0)]
    |> dict.from_list
    |> Ok
  nucleotide_count.nucleotide_count(strand)
  |> should.equal(expected)
}

pub fn strand_with_multiple_nucleotides_test() {
  let strand =
    "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
  let expected =
    [#("A", 20), #("C", 12), #("G", 17), #("T", 21)]
    |> dict.from_list
    |> Ok
  nucleotide_count.nucleotide_count(strand)
  |> should.equal(expected)
}

pub fn strand_with_invalid_nucleotides_test() {
  let strand = "AGXXACT"
  let expected = Error(Nil)
  nucleotide_count.nucleotide_count(strand)
  |> should.equal(expected)
}
