import exercism/should
import exercism/test_runner
import protein_translation

pub fn main() {
  test_runner.main()
}

// Tracks may choose to expect an error when an empty RNA sequence  
// is given, or use this test to expect an empty result.            
pub fn empty_rna_sequence_results_in_no_proteins_test() {
  protein_translation.proteins("")
  |> should.equal(Ok([]))
}

pub fn methionine_rna_sequence_test() {
  protein_translation.proteins("AUG")
  |> should.equal(Ok(["Methionine"]))
}

pub fn phenylalanine_rna_sequence_1_test() {
  protein_translation.proteins("UUU")
  |> should.equal(Ok(["Phenylalanine"]))
}

pub fn phenylalanine_rna_sequence_2_test() {
  protein_translation.proteins("UUC")
  |> should.equal(Ok(["Phenylalanine"]))
}

pub fn leucine_rna_sequence_1_test() {
  protein_translation.proteins("UUA")
  |> should.equal(Ok(["Leucine"]))
}

pub fn leucine_rna_sequence_2_test() {
  protein_translation.proteins("UUG")
  |> should.equal(Ok(["Leucine"]))
}

pub fn serine_rna_sequence_1_test() {
  protein_translation.proteins("UCU")
  |> should.equal(Ok(["Serine"]))
}

pub fn serine_rna_sequence_2_test() {
  protein_translation.proteins("UCC")
  |> should.equal(Ok(["Serine"]))
}

pub fn serine_rna_sequence_3_test() {
  protein_translation.proteins("UCA")
  |> should.equal(Ok(["Serine"]))
}

pub fn serine_rna_sequence_4_test() {
  protein_translation.proteins("UCG")
  |> should.equal(Ok(["Serine"]))
}

pub fn tyrosine_rna_sequence_1_test() {
  protein_translation.proteins("UAU")
  |> should.equal(Ok(["Tyrosine"]))
}

pub fn tyrosine_rna_sequence_2_test() {
  protein_translation.proteins("UAC")
  |> should.equal(Ok(["Tyrosine"]))
}

pub fn cysteine_rna_sequence_1_test() {
  protein_translation.proteins("UGU")
  |> should.equal(Ok(["Cysteine"]))
}

pub fn cysteine_rna_sequence_2_test() {
  protein_translation.proteins("UGC")
  |> should.equal(Ok(["Cysteine"]))
}

pub fn tryptophan_rna_sequence_test() {
  protein_translation.proteins("UGG")
  |> should.equal(Ok(["Tryptophan"]))
}

pub fn stop_codon_rna_sequence_1_test() {
  protein_translation.proteins("UAA")
  |> should.equal(Ok([]))
}

pub fn stop_codon_rna_sequence_2_test() {
  protein_translation.proteins("UAG")
  |> should.equal(Ok([]))
}

pub fn stop_codon_rna_sequence_3_test() {
  protein_translation.proteins("UGA")
  |> should.equal(Ok([]))
}

pub fn sequence_of_two_protein_codons_translates_into_proteins_test() {
  protein_translation.proteins("UUUUUU")
  |> should.equal(Ok(["Phenylalanine", "Phenylalanine"]))
}

// The codons are different, the proteins they translate into are not.
pub fn sequence_of_two_different_protein_codons_translates_into_proteins_test() {
  protein_translation.proteins("UUAUUG")
  |> should.equal(Ok(["Leucine", "Leucine"]))
}

pub fn translate_rna_strand_into_correct_protein_list_test() {
  protein_translation.proteins("AUGUUUUGG")
  |> should.equal(Ok(["Methionine", "Phenylalanine", "Tryptophan"]))
}

pub fn translation_stops_if_stop_codon_at_beginning_of_sequence_test() {
  protein_translation.proteins("UAGUGG")
  |> should.equal(Ok([]))
}

pub fn translation_stops_if_stop_codon_at_end_of_two_codon_sequence_test() {
  protein_translation.proteins("UGGUAG")
  |> should.equal(Ok(["Tryptophan"]))
}

pub fn translation_stops_if_stop_codon_at_end_of_three_codon_sequence_test() {
  protein_translation.proteins("AUGUUUUAA")
  |> should.equal(Ok(["Methionine", "Phenylalanine"]))
}

pub fn translation_stops_if_stop_codon_in_middle_of_three_codon_sequence_test() {
  protein_translation.proteins("UGGUAGUGG")
  |> should.equal(Ok(["Tryptophan"]))
}

pub fn translation_stops_if_stop_codon_in_middle_of_six_codon_sequence_test() {
  protein_translation.proteins("UGGUGUUAUUAAUGGUUU")
  |> should.equal(Ok(["Tryptophan", "Cysteine", "Tyrosine"]))
}

pub fn sequence_of_two_non_stop_codons_does_not_translate_to_a_stop_codon_test() {
  protein_translation.proteins("AUGAUG")
  |> should.equal(Ok(["Methionine", "Methionine"]))
}

// It's up to a track how to behave when a non-existing codon is    
// passed; to simplify the exercise, this test may be excluded in   
// order to remove the requirement for input validation.            
pub fn non_existing_codon_can_t_translate_test() {
  protein_translation.proteins("AAA")
  |> should.equal(Error("Invalid codon"))
}

// This test reimplements the test with uuid 1e75ea2a-f907-4994-ae5c-118632a1cb0f
// Please identify that test and remove it. Link:
// https://github.com/exercism/problem-specifications/blob/main/exercises/protein-translation/canonical-data.json
// It's up to a track how to behave when a character is encountered 
// that doesn't represent one of the amino acids part of one of the 
// codons. To simplify the exercise, this test may be excluded in   
// order to remove the requirement for input validation.            
pub fn unknown_amino_acids_not_part_of_a_codon_can_t_translate_test() {
  protein_translation.proteins("XYZ")
  |> should.equal(Error("Invalid codon"))
}

// It's up to a track how to behave when a sequence is incomplete.  
// To simplify the exercise, this test may be excluded in order to  
// remove the requirement for input validation.                     
pub fn incomplete_rna_sequence_can_t_translate_test() {
  protein_translation.proteins("AUGU")
  |> should.equal(Error("Invalid codon"))
}

// It's up to a track how to behave when a sequence is incomplete.  
// To simplify the exercise, this test may be excluded in order to  
// allow implementations to validate the strand before translating  
// it into proteins.                                                
pub fn incomplete_rna_sequence_can_translate_if_valid_until_a_stop_codon_test() {
  protein_translation.proteins("UUCUUCUAAUGGU")
  |> should.equal(Ok(["Phenylalanine", "Phenylalanine"]))
}
