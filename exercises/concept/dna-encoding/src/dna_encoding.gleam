pub type Nucleotide {
  Adenine
  Cytosine
  Guanine
  Thymine
}

pub fn encode_nucleotide(nucleotide: Nucleotide) -> Int {
  todo
}

pub fn decode_nucleotide(nucleotide: Int) -> Result(Nucleotide, Nil) {
  todo
}

pub fn encode(dna: List(Nucleotide)) -> BitArray {
  todo
}

pub fn decode(dna: BitArray) -> Result(List(Nucleotide), Nil) {
  todo
}
