import exercism/should
import exercism/test_runner
import roman_numerals

pub fn main() {
  test_runner.main()
}

pub fn number_1_is_i_test() {
  roman_numerals.convert(1)
  |> should.equal("I")
}

pub fn number_2_is_ii_test() {
  roman_numerals.convert(2)
  |> should.equal("II")
}

pub fn number_3_is_iii_test() {
  roman_numerals.convert(3)
  |> should.equal("III")
}

// Because 4 is 5 - 1
pub fn number_4_is_iv_test() {
  roman_numerals.convert(4)
  |> should.equal("IV")
}

pub fn number_5_is_v_test() {
  roman_numerals.convert(5)
  |> should.equal("V")
}

// Because 6 is 5 + 1
pub fn number_6_is_vi_test() {
  roman_numerals.convert(6)
  |> should.equal("VI")
}

// Because 9 is 10 - 1
pub fn number_9_is_ix_test() {
  roman_numerals.convert(9)
  |> should.equal("IX")
}

// 16 has exactly one occurence of all symbols whose value is less than 50 (L) in decreasing order
pub fn number_16_is_xvi_test() {
  roman_numerals.convert(16)
  |> should.equal("XVI")
}

pub fn number_27_is_xxvii_test() {
  roman_numerals.convert(27)
  |> should.equal("XXVII")
}

// Because 48 is not 50 - 2, but rather 40 + 8
pub fn number_48_is_xlviii_test() {
  roman_numerals.convert(48)
  |> should.equal("XLVIII")
}

// Because 49 is not 40 + 5 + 4, but rather (50 - 10) + (10 - 1)
pub fn number_49_is_xlix_test() {
  roman_numerals.convert(49)
  |> should.equal("XLIX")
}

pub fn number_59_is_lix_test() {
  roman_numerals.convert(59)
  |> should.equal("LIX")
}

// 66 has exactly one occurence of all symbols whose value is less than 100 (C) in decreasing order
pub fn number_66_is_lxvi_test() {
  roman_numerals.convert(66)
  |> should.equal("LXVI")
}

// Because 90 is 100 - 10
pub fn number_93_is_xciii_test() {
  roman_numerals.convert(93)
  |> should.equal("XCIII")
}

pub fn number_141_is_cxli_test() {
  roman_numerals.convert(141)
  |> should.equal("CXLI")
}

// Because 60 is 50 + 10
pub fn number_163_is_clxiii_test() {
  roman_numerals.convert(163)
  |> should.equal("CLXIII")
}

// 166 has exactly one occurence of all symbols whose value is less than 500 (D) in decreasing order
pub fn number_166_is_clxvi_test() {
  roman_numerals.convert(166)
  |> should.equal("CLXVI")
}

// Because 400 is 500 - 100
pub fn number_402_is_cdii_test() {
  roman_numerals.convert(402)
  |> should.equal("CDII")
}

pub fn number_575_is_dlxxv_test() {
  roman_numerals.convert(575)
  |> should.equal("DLXXV")
}

// 666 has exactly one occurence of all symbols whose value is less than 1000 (M) in decreasing order
pub fn number_666_is_dclxvi_test() {
  roman_numerals.convert(666)
  |> should.equal("DCLXVI")
}

// Because 900 is 1000 - 100
pub fn number_911_is_cmxi_test() {
  roman_numerals.convert(911)
  |> should.equal("CMXI")
}

pub fn number_1024_is_mxxiv_test() {
  roman_numerals.convert(1024)
  |> should.equal("MXXIV")
}

// 1666 has exactly one occurence of all symbols in decreasing order
pub fn number_1666_is_mdclxvi_test() {
  roman_numerals.convert(1666)
  |> should.equal("MDCLXVI")
}

pub fn number_3000_is_mmm_test() {
  roman_numerals.convert(3000)
  |> should.equal("MMM")
}

pub fn number_3001_is_mmmi_test() {
  roman_numerals.convert(3001)
  |> should.equal("MMMI")
}

// This is the longest possible output
pub fn number_3888_is_mmmdccclxxxviii_test() {
  roman_numerals.convert(3888)
  |> should.equal("MMMDCCCLXXXVIII")
}

pub fn number_3999_is_mmmcmxcix_test() {
  roman_numerals.convert(3999)
  |> should.equal("MMMCMXCIX")
}
