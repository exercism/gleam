import gleam/string

pub opaque type TreasureChest(treasure) {
  TreasureChest(password: String, contents: treasure)
}

pub fn create(
  password: String,
  contents: treasure,
) -> Result(TreasureChest(treasure), String) {
  case string.length(password) {
    i if i < 8 -> Error("Password must be at least 8 characters long")
    _ -> Ok(TreasureChest(password, contents))
  }
}

pub fn open(
  chest: TreasureChest(treasure),
  password: String,
) -> Result(treasure, String) {
  case password == chest.password {
    True -> Ok(chest.contents)
    False -> Error("Incorrect password")
  }
}
