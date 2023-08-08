# Instructions

Your nostalgia for Blorkemon™️ cards is showing no sign of slowing down, you even started collecting them again, and you are getting your friends to join you. 

In this exercise, a card collection is represented by `Set(String)`, since duplicate cards are not important when your goal is to get all existing cards.

## 1. Start a collection

You really want your friends to join your Blorkemon™️ madness, and the best way is to kickstart their collection by giving them one card.

Implement `new_collection`, which transforms a card into a collection.

```gleam
new_collection("Newthree")
// -> set.from_list(["Newthree"])
```

## 2. Grow the collection

Once you have a collection, it takes a life of its owm and must grow.

Implement `add_card`, which takes a card and a collection, and returns a tuple with two values: a `Bool` that indicates if the card was already in the collection, and the collection with the card added.

```gleam
add_card("Scientuna" set.from_list(["Newthree"]))
// -> #(False, set.from_list(["Newthree", "Scientuna"]))
```

## 3. Start trading

Now that your friends are Blorkemon™️ crazy again, you can use this to grow your own collection by trading cards.

Not every trade is worth doing, or can be done at all.
You cannot trade a card you don't have, and you shouldn't trade a card for one that you already have. 

Implement `trade_card`, that takes two cards to trade (yours and theirs) and your current collection.
The return value is a tuple of two values: a `Bool` stating if the trade is possible and worth doing, and the collection you would end up with if you did the trade (even if it's not actually possible).

```gleam
trade_card("Scientuna", "Newthree", set.from_list(["Scientuna"]))
// -> #(True, set.from_list(["Newthree"]))
```

## 4. Cards they all have

You and your Blorkemon™️ enthusiast friends gather and wonder which cards are the most common.

Implement `boring_cards`, which takes a list of collections and returns a list of sorted cards that all collections have.

```gleam
boring_cards([set.from_list(["Scientuna"]), set.from_list(["Newthree", "Scientuna"])])
// -> ["Scientuna"]
```

## 5. All of the cards

Do you and your friends collectively own all of the Blorkemon™️ cards?

Implement `totalCards`, which takes a list of collections and returns the total number of different cards in the all of the collections.

```elm
totalCards [set.from_list(["Scientuna"]), set.from_list(["Newthree", "Scientuna"])]
// -> 2
```

## 6. Shiny for the win

You nephew is coming to visit you soon, and you feel like impressing him.
Kids like shiny things right?
Blorkemon™️ cards can be shiny!

Implement `shiny_cards`, which takes a collection and returns a set containing all the cards that start with `"Shiny "`.

```gleam
shiny_cards(set.from_list(["Newthree", "Scientuna", "Shiny Scientuna"]))
// -> set.from_list(["Shiny Scientuna"])
```
