import exercism/should
import exercism/test_runner
import roman_numerals.{convert}

pub fn main() {
  test_runner.main()
}

pub fn number_1_is_i_test() {
  convert(1)
  |> should.equal("I")
}

pub fn number_2_is_ii_test() {
  convert(2)
  |> should.equal("II")
}

pub fn number_3_is_iii_test() {
  convert(3)
  |> should.equal("III")
}

pub fn number_4_is_iv_test() {
  convert(4)
  |> should.equal("IV")
}

pub fn number_5_is_v_test() {
  convert(5)
  |> should.equal("V")
}

pub fn number_6_is_vi_test() {
  convert(6)
  |> should.equal("VI")
}

pub fn number_9_is_ix_test() {
  convert(9)
  |> should.equal("IX")
}

pub fn number_16_is_xvi_test() {
  convert(16)
  |> should.equal("XVI")
}

pub fn number_27_is_xxvii_test() {
  convert(27)
  |> should.equal("XXVII")
}

pub fn number_48_is_xlviii_test() {
  convert(48)
  |> should.equal("XLVIII")
}

pub fn number_49_is_xlix_test() {
  convert(49)
  |> should.equal("XLIX")
}

pub fn number_59_is_lix_test() {
  convert(59)
  |> should.equal("LIX")
}

pub fn number_66_is_lxvi_test() {
  convert(66)
  |> should.equal("LXVI")
}

pub fn number_93_is_xciii_test() {
  convert(93)
  |> should.equal("XCIII")
}

pub fn number_141_is_cxli_test() {
  convert(141)
  |> should.equal("CXLI")
}

pub fn number_163_is_clxiii_test() {
  convert(163)
  |> should.equal("CLXIII")
}

pub fn number_166_is_clxvi_test() {
  convert(166)
  |> should.equal("CLXVI")
}

pub fn number_402_is_cdii_test() {
  convert(402)
  |> should.equal("CDII")
}

pub fn number_575_is_dlxxv_test() {
  convert(575)
  |> should.equal("DLXXV")
}

pub fn number_666_is_dclxvi_test() {
  convert(666)
  |> should.equal("DCLXVI")
}

pub fn number_911_is_cmxi_test() {
  convert(911)
  |> should.equal("CMXI")
}

pub fn number_1024_is_mxxiv_test() {
  convert(1024)
  |> should.equal("MXXIV")
}

pub fn number_1666_is_mdclxvi_test() {
  convert(1666)
  |> should.equal("MDCLXVI")
}

pub fn number_3000_is_mmm_test() {
  convert(3000)
  |> should.equal("MMM")
}

pub fn number_3001_is_mmmi_test() {
  convert(3001)
  |> should.equal("MMMI")
}

pub fn number_3999_is_mmmcmxcix_test() {
  convert(3999)
  |> should.equal("MMMCMXCIX")
}
