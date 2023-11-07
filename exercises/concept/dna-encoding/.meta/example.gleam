import gleam/list

pub type Nucleotide {
  Adenine
  Cytosine
  Guanine
  Thymine
}

pub fn encode_nucleotide(nucleotide: Nucleotide) -> Int {
  case nucleotide {
    Adenine -> 0b00
    Cytosine -> 0b01
    Guanine -> 0b10
    Thymine -> 0b11
  }
}

pub fn decode_nucleotide(nucleotide: Int) -> Result(Nucleotide, Nil) {
  case nucleotide {
    0b00 -> Ok(Adenine)
    0b01 -> Ok(Cytosine)
    0b10 -> Ok(Guanine)
    0b11 -> Ok(Thymine)
    _ -> Error(Nil)
  }
}

pub fn encode(dna: List(Nucleotide)) -> BitString {
  do_encode(dna, <<>>)
}

fn do_encode(dna: List(Nucleotide), acc: BitString) -> BitString {
  case dna {
    [] -> acc
    [nucleotide, ..rest] ->
      do_encode(rest, <<acc:bits, encode_nucleotide(nucleotide):2>>)
  }
}

pub fn decode(dna: BitString) -> Result(List(Nucleotide), Nil) {
  do_decode(dna, [])
}

fn do_decode(
  dna: BitString,
  acc: List(Nucleotide),
) -> Result(List(Nucleotide), Nil) {
  case dna {
    <<>> -> Ok(list.reverse(acc))
    <<0b00:2, rest:bits>> -> do_decode(rest, [Adenine, ..acc])
    <<0b01:2, rest:bits>> -> do_decode(rest, [Cytosine, ..acc])
    <<0b10:2, rest:bits>> -> do_decode(rest, [Guanine, ..acc])
    <<0b11:2, rest:bits>> -> do_decode(rest, [Thymine, ..acc])
    _ -> Error(Nil)
  }
}
