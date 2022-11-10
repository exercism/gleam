import roman_numerals.{convert}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn number_1_test() {
  convert(1)
  |> should.equal("I")
}

pub fn number_2_test() {
  convert(2)
  |> should.equal("II")
}

pub fn number_3_test() {
  convert(3)
  |> should.equal("III")
}

pub fn number_4_test() {
  convert(4)
  |> should.equal("IV")
}

pub fn number_5_test() {
  convert(5)
  |> should.equal("V")
}

pub fn number_6_test() {
  convert(6)
  |> should.equal("VI")
}

pub fn number_9_test() {
  convert(9)
  |> should.equal("IX")
}

pub fn number_16_test() {
  convert(16)
  |> should.equal("XVI")
}

pub fn number_27_test() {
  convert(27)
  |> should.equal("XXVII")
}

pub fn number_32_test() {
  convert(32)
  |> should.equal("XXXII")
}

pub fn number_39_test() {
  convert(39)
  |> should.equal("XXXIX")
}

pub fn number_48_test() {
  convert(48)
  |> should.equal("XLVIII")
}

pub fn number_49_test() {
  convert(49)
  |> should.equal("XLIX")
}

pub fn number_59_test() {
  convert(59)
  |> should.equal("LIX")
}

pub fn number_64_test() {
  convert(64)
  |> should.equal("LXIV")
}

pub fn number_93_test() {
  convert(93)
  |> should.equal("XCIII")
}

pub fn number_128_test() {
  convert(128)
  |> should.equal("CXXVIII")
}

pub fn number_141_test() {
  convert(141)
  |> should.equal("CXLI")
}

pub fn number_163_test() {
  convert(163)
  |> should.equal("CLXIII")
}

pub fn number_246_test() {
  convert(246)
  |> should.equal("CCXLVI")
}

pub fn number_402_test() {
  convert(402)
  |> should.equal("CDII")
}

pub fn number_575_test() {
  convert(575)
  |> should.equal("DLXXV")
}

pub fn number_666_test() {
  convert(666)
  |> should.equal("DCLXVI")
}

pub fn number_789_test() {
  convert(789)
  |> should.equal("DCCLXXXIX")
}

pub fn number_911_test() {
  convert(911)
  |> should.equal("CMXI")
}

pub fn number_1024_test() {
  convert(1024)
  |> should.equal("MXXIV")
}

pub fn number_1666_test() {
  convert(1666)
  |> should.equal("MDCLXVI")
}

pub fn number_1776_test() {
  convert(1776)
  |> should.equal("MDCCLXXVI")
}

pub fn number_1918_test() {
  convert(1918)
  |> should.equal("MCMXVIII")
}

pub fn number_1954_test() {
  convert(1954)
  |> should.equal("MCMLIV")
}

pub fn number_2421_test() {
  convert(2421)
  |> should.equal("MMCDXXI")
}

pub fn number_3000_test() {
  convert(3000)
  |> should.equal("MMM")
}
