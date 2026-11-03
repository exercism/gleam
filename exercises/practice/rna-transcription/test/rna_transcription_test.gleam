import exercism/should
import exercism/test_runner
import rna_transcription

pub fn main() {
  test_runner.main()
}

pub fn empty_rna_sequence_test() {
  ""
  |> rna_transcription.to_rna
  |> should.equal(Ok(""))
}

pub fn rna_complement_of_cytosine_is_guanine_test() {
  "C"
  |> rna_transcription.to_rna
  |> should.equal(Ok("G"))
}

pub fn rna_complement_of_guanine_is_cytosine_test() {
  "G"
  |> rna_transcription.to_rna
  |> should.equal(Ok("C"))
}

pub fn rna_complement_of_thymine_is_adenine_test() {
  "T"
  |> rna_transcription.to_rna
  |> should.equal(Ok("A"))
}

pub fn rna_complement_of_adenine_is_uracil_test() {
  "A"
  |> rna_transcription.to_rna
  |> should.equal(Ok("U"))
}

pub fn rna_complement_test() {
  "ACGTGGTCTTAA"
  |> rna_transcription.to_rna
  |> should.equal(Ok("UGCACCAGAAUU"))
}

pub fn invalid_dna_test() {
  "INVALID"
  |> rna_transcription.to_rna
  |> should.equal(Error(Nil))
}
