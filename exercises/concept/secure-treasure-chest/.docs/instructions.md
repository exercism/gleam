# Instructions

Sharp eyed students will have noticed that the `TreasureChest` type in the previous exercise wasn't that secure!

If you used the `get_treasure` function you had to supply the password, but you could still destructure the `TreasureChest` type to get the treasure without having to know the password.

Let's fix that by using an Opaque Type.

## 1. Define the `TreasureChest` opaque type.

The `TreasureChest` contains two fields:
- A password that is a `String`.
- A treasure that is a generic type.

The `TreasureChest` type must be opaque.

## 2. Define the `create` function.

This function takes two arguments:
- A password `String`.
- A treasure value of any type.

The function returns a `TreasureChest` containing the password and the value.

If the password is shorter than 8 characters then the function should return an error saying `Password must be at least 8 characters long`.

## 3. Define `open` function.

This function takes two arguments:

- A `TreasureChest`.
- A password `String`.

If the password matches the password in the `TreasureChest` then the function should return the treasure, otherwise it should return an error saying `Incorrect password`.
