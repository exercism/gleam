import exercism/should
import exercism/test_runner
import protein_translation

pub fn main() {
  test_runner.main()
}

pub fn empty_rna_sequence_results_in_no_proteins_test() {
  ""
  |> protein_translation.proteins
  |> should.equal(Ok([]))
}

pub fn methione_rna_sequence_test() {
  "AUG"
  |> protein_translation.proteins
  |> should.equal(Ok(["Methionine"]))
}

pub fn phenylalanine_rna_sequence_1_test() {
  "UUU"
  |> protein_translation.proteins
  |> should.equal(Ok(["Phenylalanine"]))
}

pub fn phenylalanine_rna_sequence_2_test() {
  "UUC"
  |> protein_translation.proteins
  |> should.equal(Ok(["Phenylalanine"]))
}

pub fn leucine_rna_sequence_1_test() {
  "UUA"
  |> protein_translation.proteins
  |> should.equal(Ok(["Leucine"]))
}

pub fn leucine_rna_sequence_2_test() {
  "UUG"
  |> protein_translation.proteins
  |> should.equal(Ok(["Leucine"]))
}

pub fn serine_rna_sequence_1_test() {
  "UCU"
  |> protein_translation.proteins
  |> should.equal(Ok(["Serine"]))
}

pub fn serine_rna_sequence_2_test() {
  "UCC"
  |> protein_translation.proteins
  |> should.equal(Ok(["Serine"]))
}

pub fn serine_rna_sequence_3_test() {
  "UCA"
  |> protein_translation.proteins
  |> should.equal(Ok(["Serine"]))
}

pub fn serine_rna_sequence_4_test() {
  "UCG"
  |> protein_translation.proteins
  |> should.equal(Ok(["Serine"]))
}

pub fn tyrosine_rna_sequence_1_test() {
  "UAU"
  |> protein_translation.proteins
  |> should.equal(Ok(["Tyrosine"]))
}

pub fn tyrosine_rna_sequence_2_test() {
  "UAC"
  |> protein_translation.proteins
  |> should.equal(Ok(["Tyrosine"]))
}

pub fn cysteine_rna_sequence_1_test() {
  "UGU"
  |> protein_translation.proteins
  |> should.equal(Ok(["Cysteine"]))
}

pub fn cysteine_rna_sequence_2_test() {
  "UGC"
  |> protein_translation.proteins
  |> should.equal(Ok(["Cysteine"]))
}

pub fn tryptophan_rna_sequence_test() {
  "UGG"
  |> protein_translation.proteins
  |> should.equal(Ok(["Tryptophan"]))
}

pub fn stop_codon_rna_sequence_1_test() {
  "UAA"
  |> protein_translation.proteins
  |> should.equal(Ok([]))
}

pub fn stop_codon_rna_sequence_2_test() {
  "UAG"
  |> protein_translation.proteins
  |> should.equal(Ok([]))
}

pub fn stop_codon_rna_sequence_3_test() {
  "UGA"
  |> protein_translation.proteins
  |> should.equal(Ok([]))
}

pub fn sequence_of_two_protein_codons_translates_into_proteins_test() {
  "UUUUUU"
  |> protein_translation.proteins
  |> should.equal(Ok(["Phenylalanine", "Phenylalanine"]))
}

pub fn sequence_of_two_different_protein_codons_translates_into_proteins_test() {
  "UUAUUG"
  |> protein_translation.proteins
  |> should.equal(Ok(["Leucine", "Leucine"]))
}

pub fn translate_rna_strand_into_correct_protein_list_test() {
  "AUGUUUUGG"
  |> protein_translation.proteins
  |> should.equal(Ok(["Methionine", "Phenylalanine", "Tryptophan"]))
}

pub fn translation_stops_if_stop_codon_at_beginning_of_sequence_test() {
  "UAGUGG"
  |> protein_translation.proteins
  |> should.equal(Ok([]))
}

pub fn translation_stops_if_stop_codon_at_end_of_two_codon_sequence_test() {
  "UGGUAG"
  |> protein_translation.proteins
  |> should.equal(Ok(["Tryptophan"]))
}

pub fn translation_stops_if_stop_codon_at_end_of_three_codon_sequence_test() {
  "AUGUUUUAA"
  |> protein_translation.proteins
  |> should.equal(Ok(["Methionine", "Phenylalanine"]))
}

pub fn translation_stops_if_stop_codon_in_middle_of_three_codon_sequence_test() {
  "UGGUAGUGG"
  |> protein_translation.proteins
  |> should.equal(Ok(["Tryptophan"]))
}

pub fn non_existing_codon_cant_translate_test() {
  "AAA"
  |> protein_translation.proteins
  |> should.equal(Error(Nil))
}

pub fn unknown_amino_acids_not_part_of_a_codon_cant_translate_test() {
  "XYZ"
  |> protein_translation.proteins
  |> should.equal(Error(Nil))
}

pub fn incomplete_rna_sequence_cant_translate_test() {
  "AUGU"
  |> protein_translation.proteins
  |> should.equal(Error(Nil))
}

pub fn translation_stops_if_stop_codon_in_middle_of_six_codon_sequence_test() {
  "UGGUGUUAUUAAUGGUUU"
  |> protein_translation.proteins
  |> should.equal(Ok(["Tryptophan", "Cysteine", "Tyrosine"]))
}

pub fn incomplete_rna_sequence_can_translate_if_valid_until_a_stop_codon_test() {
  "UUCUUCUAAUGGU"
  |> protein_translation.proteins
  |> should.equal(Ok(["Phenylalanine", "Phenylalanine"]))
}
