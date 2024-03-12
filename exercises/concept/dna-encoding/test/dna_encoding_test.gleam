import dna_encoding.{Adenine, Cytosine, Guanine, Thymine}
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn encode_nucleotide_adenine_test() {
  Adenine
  |> dna_encoding.encode_nucleotide
  |> should.equal(0b00)
}

pub fn encode_nucleotide_cytosine_test() {
  Cytosine
  |> dna_encoding.encode_nucleotide
  |> should.equal(0b01)
}

pub fn encode_nucleotide_guanine_test() {
  Guanine
  |> dna_encoding.encode_nucleotide
  |> should.equal(0b10)
}

pub fn encode_nucleotide_thymine_test() {
  Thymine
  |> dna_encoding.encode_nucleotide
  |> should.equal(0b11)
}

pub fn decode_nucleotide_0b00_test() {
  0b00
  |> dna_encoding.decode_nucleotide
  |> should.equal(Ok(Adenine))
}

pub fn decode_nucleotide_0b01_test() {
  0b01
  |> dna_encoding.decode_nucleotide
  |> should.equal(Ok(Cytosine))
}

pub fn decode_nucleotide_0b10_test() {
  0b10
  |> dna_encoding.decode_nucleotide
  |> should.equal(Ok(Guanine))
}

pub fn decode_nucleotide_0b11_test() {
  0b11
  |> dna_encoding.decode_nucleotide
  |> should.equal(Ok(Thymine))
}

pub fn decode_nucleotide_0b100_test() {
  0b100
  |> dna_encoding.decode_nucleotide
  |> should.equal(Error(Nil))
}

pub fn encode_a_test() {
  [Adenine]
  |> dna_encoding.encode
  |> should.equal(<<0b00:2>>)
}

pub fn encode_c_test() {
  [Cytosine]
  |> dna_encoding.encode
  |> should.equal(<<0b01:2>>)
}

pub fn encode_g_test() {
  [Guanine]
  |> dna_encoding.encode
  |> should.equal(<<0b10:2>>)
}

pub fn encode_t_test() {
  [Thymine]
  |> dna_encoding.encode
  |> should.equal(<<0b11:2>>)
}

pub fn encode_acgt_test() {
  [Adenine, Cytosine, Guanine, Thymine]
  |> dna_encoding.encode
  |> should.equal(<<0b00011011:8>>)
}

pub fn encode_tgca_test() {
  [Thymine, Guanine, Cytosine, Adenine]
  |> dna_encoding.encode
  |> should.equal(<<0b11100100:8>>)
}

pub fn decode_a_test() {
  <<0b00:2>>
  |> dna_encoding.decode
  |> should.equal(Ok([Adenine]))
}

pub fn decode_c_test() {
  <<0b01:2>>
  |> dna_encoding.decode
  |> should.equal(Ok([Cytosine]))
}

pub fn decode_g_test() {
  <<0b10:2>>
  |> dna_encoding.decode
  |> should.equal(Ok([Guanine]))
}

pub fn decode_t_test() {
  <<0b11:2>>
  |> dna_encoding.decode
  |> should.equal(Ok([Thymine]))
}

pub fn decode_acgt_test() {
  <<0b00011011:8>>
  |> dna_encoding.decode
  |> should.equal(Ok([Adenine, Cytosine, Guanine, Thymine]))
}

pub fn decode_tgca_test() {
  <<0b11100100:8>>
  |> dna_encoding.decode
  |> should.equal(Ok([Thymine, Guanine, Cytosine, Adenine]))
}

pub fn decode_invalid_test() {
  <<0b0000000:7>>
  |> dna_encoding.decode
  |> should.equal(Error(Nil))
}
