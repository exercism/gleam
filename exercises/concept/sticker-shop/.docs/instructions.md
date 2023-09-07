# Instructions

Lucy has an online sticker shop, where she sells cute stickers featuring everyone's favourite programming languages. People from all around the world buy her stickers, and she's having some trouble dealing with all the different currencies.

Create a program that Lucy can use calculate prices while being sure that she's always using the correct currency.

## 1. Define the `Usd`, `Eur`, and `Jpy` types

These type are used to represent the different currencies that Lucy's customers use to buy her stickers.

They are to be used as phantom types and do not need to have any constructors.

## 2. Define the `Money` type.

The `Money` type should have an `Int` field for the amount of money, a currency phantom type parameter, and it should be an opaque type.

## 3. Define `dollar`, `euro`, and `yen` functions

Define the `dollar`, `euro`, and `yen` functions that take an `Int` argument and return a `Money` value with the correct currency.

## 4. Define the `total` function

Define the `total` function which takes a list of `Money` values and returns the total amount of money in the list.

```gleam
total([euro(120), euro(200), yen(145)])
// -> euro(465)
```
