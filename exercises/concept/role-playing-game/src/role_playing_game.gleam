import gleam/option.{type Option}

pub type Player {
  Player(name: Option(String), level: Int, health: Int, mana: Option(Int))
}

pub fn introduce(player: Player) -> String {
  todo
}

pub fn revive(player: Player) -> Option(Player) {
  todo
}

pub fn cast_spell(player: Player, cost: Int) -> #(Player, Int) {
  todo
}
