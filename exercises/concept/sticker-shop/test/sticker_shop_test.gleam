import exercism/should
import exercism/test_runner
import gleam/string
import simplifile
import sticker_shop.{type Eur, type Jpy, type Money, type Usd}

pub fn main() {
  test_runner.main()
}

fn compact_whitespace(graphemes: List(String), acc: String) -> String {
  case graphemes {
    [] -> acc
    [" ", " ", ..rest] -> compact_whitespace([" ", ..rest], acc)
    [grapheme, ..rest] -> compact_whitespace(rest, acc <> grapheme)
  }
}

fn read_source() -> String {
  let assert Ok(src) = simplifile.read("src/sticker_shop.gleam")
  src
  |> string.replace("\n", "")
  |> string.to_graphemes
  |> compact_whitespace("")
  |> string.replace(" (", "(")
  |> string.replace(")->", ") ->")
}

pub fn type_must_be_opaque_test() {
  let src = read_source()
  case string.contains(src, "pub opaque type Money") {
    True -> Nil
    False -> panic as "The Money type must exist and be opaque"
  }
}

pub fn dollar_function_must_return_usd_test() {
  let src = read_source()
  case string.contains(src, "-> Money(Usd)") {
    True -> Nil
    False -> panic as "The dollar function must return Usd"
  }
}

pub fn euro_function_must_return_eur_test() {
  let src = read_source()
  case string.contains(src, "-> Money(Eur)") {
    True -> Nil
    False -> panic as "The euro function must return Eur"
  }
}

pub fn yen_function_must_return_jpy_test() {
  let src = read_source()
  case string.contains(src, "-> Money(Jpy)") {
    True -> Nil
    False -> panic as "The yen function must return Jpy"
  }
}

pub fn dollar_test() {
  let _money: Money(Usd) = sticker_shop.dollar(1)
}

pub fn euro_test() {
  let _money: Money(Eur) = sticker_shop.euro(1)
}

pub fn yen_test() {
  let _money: Money(Jpy) = sticker_shop.yen(1)
}

pub fn total_dollars_test() {
  [
    sticker_shop.dollar(120),
    sticker_shop.dollar(50),
    sticker_shop.dollar(45),
    sticker_shop.dollar(100),
  ]
  |> sticker_shop.total
  |> should.equal(sticker_shop.dollar(315))
}

pub fn total_euros_test() {
  [
    sticker_shop.euro(110),
    sticker_shop.euro(20),
    sticker_shop.euro(35),
    sticker_shop.euro(100),
  ]
  |> sticker_shop.total
  |> should.equal(sticker_shop.euro(265))
}

pub fn total_yen_test() {
  [
    sticker_shop.yen(480),
    sticker_shop.yen(340),
    sticker_shop.yen(455),
    sticker_shop.yen(165),
    sticker_shop.yen(100),
  ]
  |> sticker_shop.total
  |> should.equal(sticker_shop.yen(1540))
}
