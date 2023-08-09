import gleam/option.{None, Option, Some}
import gleam/int

pub type Player {
  Player(name: Option(String), level: Int, health: Int, mana: Option(Int))
}

pub fn introduce(player: Player) -> String {
  option.unwrap(player.name, "Mighty Magician")
}

pub fn revive(player: Player) -> Option(Player) {
  case player {
    Player(health: 0, level: level, ..) if level >= 10 ->
      Some(Player(..player, health: 100, mana: Some(100)))

    Player(health: 0, ..) -> Some(Player(..player, health: 100))

    _ -> None
  }
}

pub fn cast_spell(player: Player, cost: Int) -> #(Player, Int) {
  case player {
    Player(mana: Some(mana), ..) if mana >= cost -> {
      let player = Player(..player, mana: Some(mana - cost))
      #(player, cost * 2)
    }

    Player(health: health, mana: None, ..) -> {
      let player = Player(..player, health: int.max(0, health - cost))
      #(player, 0)
    }

    _ -> #(player, 0)
  }
}
