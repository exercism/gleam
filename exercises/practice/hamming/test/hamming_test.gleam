import exercism/should
import exercism/test_runner
import hamming

pub fn main() {
  test_runner.main()
}

pub fn empty_strands_test() {
  hamming.distance("", "")
  |> should.equal(Ok(0))
}

pub fn single_letter_identical_strands_test() {
  hamming.distance("A", "A")
  |> should.equal(Ok(0))
}

pub fn single_letter_different_strands_test() {
  hamming.distance("G", "T")
  |> should.equal(Ok(1))
}

pub fn long_identical_strands_test() {
  hamming.distance("GGACTGAAATCTG", "GGACTGAAATCTG")
  |> should.equal(Ok(0))
}

pub fn long_different_strands_test() {
  hamming.distance("GGACGGATTCTG", "AGGACGGATTCT")
  |> should.equal(Ok(9))
}

pub fn disallow_first_strand_longer_test() {
  hamming.distance("AATG", "AAA")
  |> should.equal(Error(Nil))
}

pub fn disallow_second_strand_longer_test() {
  hamming.distance("ATA", "AGTG")
  |> should.equal(Error(Nil))
}

pub fn disallow_empty_first_strand_test() {
  hamming.distance("", "G")
  |> should.equal(Error(Nil))
}

pub fn disallow_empty_second_strand_test() {
  hamming.distance("G", "")
  |> should.equal(Error(Nil))
}
