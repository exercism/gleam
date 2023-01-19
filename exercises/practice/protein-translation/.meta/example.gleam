import gleam/list

pub fn proteins(rna: String) -> Result(List(String), Nil) {
  convert(rna, [])
}

fn convert(rna: String, acc: List(String)) -> Result(List(String), Nil) {
  case rna {
    "" -> Ok(list.reverse(acc))
    "UAA" <> _ -> Ok(list.reverse(acc))
    "UAG" <> _ -> Ok(list.reverse(acc))
    "UGA" <> _ -> Ok(list.reverse(acc))

    "AUG" <> rna -> convert(rna, ["Methionine", ..acc])

    "UUU" <> rna -> convert(rna, ["Phenylalanine", ..acc])
    "UUC" <> rna -> convert(rna, ["Phenylalanine", ..acc])

    "UUA" <> rna -> convert(rna, ["Leucine", ..acc])
    "UUG" <> rna -> convert(rna, ["Leucine", ..acc])

    "UCU" <> rna -> convert(rna, ["Serine", ..acc])
    "UCC" <> rna -> convert(rna, ["Serine", ..acc])
    "UCA" <> rna -> convert(rna, ["Serine", ..acc])
    "UCG" <> rna -> convert(rna, ["Serine", ..acc])

    "UAU" <> rna -> convert(rna, ["Tyrosine", ..acc])
    "UAC" <> rna -> convert(rna, ["Tyrosine", ..acc])

    "UGU" <> rna -> convert(rna, ["Cysteine", ..acc])
    "UGC" <> rna -> convert(rna, ["Cysteine", ..acc])

    "UGG" <> rna -> convert(rna, ["Tryptophan", ..acc])

    _ -> Error(Nil)
  }
}
