# Instructions

In this exercise you're going to write some code to help Betty's Bike Shop, an online shop for bikes and parts.
You have three tasks, aiming at correctly displaying the bikes and parts prices on the website.

## 1. Export the `pence_to_pounds` function

Your colleague has already written the skeleton of the module, the incomplete functions `pence_to_pounds` and `pounds_to_pence`.
However, they have forgotten to export the `pence_to_pounds` function, so it is currently not visible by other modules or by the test suite.

Export the `pence_to_pounds` function.

## 2. Convert pence to pounds

Currently, the price is stored as an integer number of *pence* (the bike shop is based in the UK).
On the website, we want to show the price in *pounds*, where 1.00 pound amounts to 100 pence.
Your first task is to implement the `pence_to_pounds` function, taking an `Int` amount of pence, and converting it into its equivalent pounds as a `Float`.
You should also add type annotations for that function.

```gleam
pence_to_pounds(106)
// -> 1.06
```

## 3. Format the price for display on the website

Since Betty's bikes are sold in pounds, prices should be displayed with the symbol "£".
Your second task is thus to implement the `pounds_to_string` function, taking an amount of pounds as a `Float` and returning its price displayed as a `String` with the pound symbol prepended.

You should import the `gleam/float` and `gleam/string` modules before using them.

You should also define the type annotation for `pounds_to_string`.

```gleam
pounds_to_string(1.06)
// -> "£1.06"
```
