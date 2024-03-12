import exercism/should
import exercism/test_runner
import raindrops

pub fn main() {
  test_runner.main()
}

pub fn the_sound_for_1_is_1_test() {
  raindrops.convert(1)
  |> should.equal("1")
}

pub fn the_sound_for_3_is_pling_test() {
  raindrops.convert(3)
  |> should.equal("Pling")
}

pub fn the_sound_for_5_is_plang_test() {
  raindrops.convert(5)
  |> should.equal("Plang")
}

pub fn the_sound_for_7_is_plong_test() {
  raindrops.convert(7)
  |> should.equal("Plong")
}

pub fn the_sound_for_6_is_pling_as_it_has_a_factor_3_test() {
  raindrops.convert(6)
  |> should.equal("Pling")
}

pub fn two_to_the_power_3_does_not_make_a_raindrop_sound_as_3_is_the_exponent_not_the_base_test() {
  raindrops.convert(8)
  |> should.equal("8")
}

pub fn the_sound_for_9_is_pling_as_it_has_a_factor_3_test() {
  raindrops.convert(9)
  |> should.equal("Pling")
}

pub fn the_sound_for_10_is_plang_as_it_has_a_factor_5_test() {
  raindrops.convert(10)
  |> should.equal("Plang")
}

pub fn the_sound_for_14_is_plong_as_it_has_a_factor_of_7_test() {
  raindrops.convert(14)
  |> should.equal("Plong")
}

pub fn the_sound_for_15_is_plingplang_as_it_has_factors_3_and_5_test() {
  raindrops.convert(15)
  |> should.equal("PlingPlang")
}

pub fn the_sound_for_21_is_plingplong_as_it_has_factors_3_and_7_test() {
  raindrops.convert(21)
  |> should.equal("PlingPlong")
}

pub fn the_sound_for_25_is_plang_as_it_has_a_factor_5_test() {
  raindrops.convert(25)
  |> should.equal("Plang")
}

pub fn the_sound_for_27_is_pling_as_it_has_a_factor_3_test() {
  raindrops.convert(27)
  |> should.equal("Pling")
}

pub fn the_sound_for_35_is_plangplong_as_it_has_factors_5_and_7_test() {
  raindrops.convert(35)
  |> should.equal("PlangPlong")
}

pub fn the_sound_for_49_is_plong_as_it_has_a_factor_7_test() {
  raindrops.convert(49)
  |> should.equal("Plong")
}

pub fn the_sound_for_52_is_52_test() {
  raindrops.convert(52)
  |> should.equal("52")
}

pub fn the_sound_for_105_is_plingplangplong_as_it_has_factors_3_5_and_7_test() {
  raindrops.convert(105)
  |> should.equal("PlingPlangPlong")
}

pub fn the_sound_for_3125_is_plang_as_it_has_a_factor_5_test() {
  raindrops.convert(3125)
  |> should.equal("Plang")
}
