pub type TreasureChest(treasure) {
  TreasureChest(String, treasure)
}

pub type UnlockResult(treasure) {
  Unlocked(treasure)
  WrongPassword
}

pub fn get_treasure(
  chest: TreasureChest(treasure),
  password: String,
) -> UnlockResult(treasure) {
  let TreasureChest(set, treasure) = chest
  case set == password {
    True -> Unlocked(treasure)
    False -> WrongPassword
  }
}
