# Hints

## General

- Try to split a problem into a base case and a recursive case. For example, let's say you want to count how many cookies are there in the cookie jar with a recursive approach. A base case is an empty jar - it has zero cookies. If the jar is not empty, then the number of cookies in the jar is equal to one cookie plus the number of cookies in the jar after removing one cookie.

## 1. Define the pizza types and options

- The `Pizza` type is a recursive type, with the `ExtraSauce` and `ExtraToppings` cases containing a `Pizza`.

## 2. Calculate the price of pizza

- To handle the `Pizza` type being a recursive type, define a recursive function.

## 3. Calculate the price of an order

- The exact length of the list can be pattern matched upon to determine if the additional fee should be applied.
- Use tail recursion to avoid using too much memory when calculating the price of an order.
