import exercism/should
import exercism/test_runner
import pig_latin

pub fn main() {
  test_runner.main()
}

pub fn ay_is_added_to_words_that_start_with_vowels_word_beginning_with_a_test() {
  pig_latin.translate("apple")
  |> should.equal("appleay")
}

pub fn ay_is_added_to_words_that_start_with_vowels_word_beginning_with_e_test() {
  pig_latin.translate("ear")
  |> should.equal("earay")
}

pub fn ay_is_added_to_words_that_start_with_vowels_word_beginning_with_i_test() {
  pig_latin.translate("igloo")
  |> should.equal("iglooay")
}

pub fn ay_is_added_to_words_that_start_with_vowels_word_beginning_with_o_test() {
  pig_latin.translate("object")
  |> should.equal("objectay")
}

pub fn ay_is_added_to_words_that_start_with_vowels_word_beginning_with_u_test() {
  pig_latin.translate("under")
  |> should.equal("underay")
}

pub fn ay_is_added_to_words_that_start_with_vowels_word_beginning_with_a_vowel_and_followed_by_a_qu_test() {
  pig_latin.translate("equal")
  |> should.equal("equalay")
}

pub fn first_letter_and_ay_are_moved_to_the_end_of_words_that_start_with_consonants_word_beginning_with_p_test() {
  pig_latin.translate("pig")
  |> should.equal("igpay")
}

pub fn first_letter_and_ay_are_moved_to_the_end_of_words_that_start_with_consonants_word_beginning_with_k_test() {
  pig_latin.translate("koala")
  |> should.equal("oalakay")
}

pub fn first_letter_and_ay_are_moved_to_the_end_of_words_that_start_with_consonants_word_beginning_with_x_test() {
  pig_latin.translate("xenon")
  |> should.equal("enonxay")
}

pub fn first_letter_and_ay_are_moved_to_the_end_of_words_that_start_with_consonants_word_beginning_with_q_without_a_following_u_test() {
  pig_latin.translate("qat")
  |> should.equal("atqay")
}

pub fn some_letter_clusters_are_treated_like_a_single_consonant_word_beginning_with_ch_test() {
  pig_latin.translate("chair")
  |> should.equal("airchay")
}

pub fn some_letter_clusters_are_treated_like_a_single_consonant_word_beginning_with_qu_test() {
  pig_latin.translate("queen")
  |> should.equal("eenquay")
}

pub fn some_letter_clusters_are_treated_like_a_single_consonant_word_beginning_with_qu_and_a_preceding_consonant_test() {
  pig_latin.translate("square")
  |> should.equal("aresquay")
}

pub fn some_letter_clusters_are_treated_like_a_single_consonant_word_beginning_with_th_test() {
  pig_latin.translate("therapy")
  |> should.equal("erapythay")
}

pub fn some_letter_clusters_are_treated_like_a_single_consonant_word_beginning_with_thr_test() {
  pig_latin.translate("thrush")
  |> should.equal("ushthray")
}

pub fn some_letter_clusters_are_treated_like_a_single_consonant_word_beginning_with_sch_test() {
  pig_latin.translate("school")
  |> should.equal("oolschay")
}

pub fn some_letter_clusters_are_treated_like_a_single_vowel_word_beginning_with_yt_test() {
  pig_latin.translate("yttria")
  |> should.equal("yttriaay")
}

pub fn some_letter_clusters_are_treated_like_a_single_vowel_word_beginning_with_xr_test() {
  pig_latin.translate("xray")
  |> should.equal("xrayay")
}

pub fn position_of_y_in_a_word_determines_if_it_is_a_consonant_or_a_vowel_y_is_treated_like_a_consonant_at_the_beginning_of_a_word_test() {
  pig_latin.translate("yellow")
  |> should.equal("ellowyay")
}

pub fn position_of_y_in_a_word_determines_if_it_is_a_consonant_or_a_vowel_y_is_treated_like_a_vowel_at_the_end_of_a_consonant_cluster_test() {
  pig_latin.translate("rhythm")
  |> should.equal("ythmrhay")
}

pub fn position_of_y_in_a_word_determines_if_it_is_a_consonant_or_a_vowel_y_as_second_letter_in_two_letter_word_test() {
  pig_latin.translate("my")
  |> should.equal("ymay")
}

pub fn phrases_are_translated_a_whole_phrase_test() {
  pig_latin.translate("quick fast run")
  |> should.equal("ickquay astfay unray")
}
