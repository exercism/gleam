# Instructions

Josh is working on a new role-playing game and needs your help implementing some of the mechanics.

## 1. Introduce yourself

Implement the `introduce` function.

Stealthy players may be hiding their name and will be introduced as `"Mighty Magician"`.
Otherwise, just use the player's name to introduce them.

```gleam
introduce(Player(name: None, level: 2, health: 8, mana: None))
// -> "Mighty Magician"

introduce(Player(name: Some("Merlin"), level: 2, health: 8, mana: None))
// -> "Merlin"
```

## 2. Implement the revive mechanic

The `revive` function should check that the player's character is indeed dead (their health has reached 0).
If they are, it should return a new `Player` instance with 100 health.
Otherwise, if the player's character isn't dead, the `revive` function returns `None`.

If the player's level is 10 or above, they should also be revived with 100 mana.
If they player's level is below 10, their mana should be `None`.
The `revive` function should preserve the player's level.

```gleam
let dead_player = Player(name: None, level: 2, health: 0, mana: None)

revive(dead_player)
// -> Some(Player(name: None, level: 2, health: 100, mana: None))
```

If the `revive` method is called on a player whose health is 1 or above, then the function should return `None`.

```gleam
let alive_player = Player(name: None, level: 2, health: 42, mana: None)

revive(alive_player)
// -> None
```

## 3. Implement the spell casting mechanic

The `cast_spell` function takes as arguments an `Int` indicating how much mana the spell costs as well as a `Player`.
It returns the updated player, as well as the amount of damage that the cast spell performs.
A successful spell cast does damage equal to two times the mana cost of the spell.
However, if the player has insufficient mana, nothing happens, the player is unchanged and no damage is done.
If the player does not even have a mana pool, attempting to cast the spell must decrease their health by the mana cost of the spell and does no damage.
Be aware that the players health cannot be below zero (0).

```gleam
let wizard = Player(name: None, level: 18, health: 123, mana: Some(30))
let #(wizard, damage) = cast_spell(wizard, 14)

wizard.mana // -> Some 16
damage      // -> 28
```
