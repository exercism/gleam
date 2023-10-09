import exercism/should
import exercism/test_runner
import gleam/string
import secure_treasure_chest
import simplifile

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
  let assert Ok(src) = simplifile.read("src/secure_treasure_chest.gleam")
  compact_whitespace(string.to_graphemes(src), "")
}

pub fn type_must_be_opaque_test() {
  let src = read_source()
  case string.contains(src, "pub opaque type TreasureChest") {
    True -> Nil
    False -> panic as "The TreasureChest type must exist and be opaque"
  }
}

pub fn create_is_ok_with_long_password_test() {
  let assert Ok(_) = secure_treasure_chest.create("12345678", "My treasure")
}

pub fn create_is_error_with_short_password_test() {
  secure_treasure_chest.create("1234567", "My treasure")
  |> should.equal(Error("Password must be at least 8 characters long"))
}

pub fn open_is_ok_with_the_correct_password_test() {
  let assert Ok(chest) = secure_treasure_chest.create("wwwibble", 100)
  secure_treasure_chest.open(chest, "wwwibble")
  |> should.equal(Ok(100))

  let assert Ok(chest) = secure_treasure_chest.create("wwwobble", 1.5)
  secure_treasure_chest.open(chest, "wwwobble")
  |> should.equal(Ok(1.5))
}

pub fn open_is_an_error_with_an_incorrect_password_test() {
  let assert Ok(chest) = secure_treasure_chest.create("wwwibble", 100)
  secure_treasure_chest.open(chest, "wwwobble")
  |> should.equal(Error("Incorrect password"))
}
