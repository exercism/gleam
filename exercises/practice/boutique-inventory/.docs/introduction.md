# Introduction

## Yielders

A `Yielder` in Gleam is similar to a list in that it is an ordered collection of values, where all values are of the same type. It differs from a list in that it is lazy, meaning that it does not store all of its values in memory at once, but instead calculates them as they are needed.

This is useful when working with sequences that are very large or even infinite in size, as it allows us to work with them when we do not have enough free memory to store them all at once.

The `gleam/yielder` module defines the `Yielder` type as well as functions for working with yielders.

Many of the functions for lists also exist for yielders, such as `map`, `filter`, and `take`. These functions return yielders, so the functions do not actually perform any work until the yielder is consumed.

```gleam
let iter =
  [1, 2, 3, 4, 5, 6]
  |> yielder.from_list
  |> yielder.filter(fn(x) { x > 2 })
  |> yielder.take(2)

// No work has been done yet as the yielder has not been consumed

// Consume the yielder and collect the values into a list
yielder.to_list(iter)
// -> [3, 4]
```

The `unfold` function can be used to create yielders from a function that is called repeatedly to produce the next value in the sequence.

Here the `unfold` function is used to create an yielder that produces the Fibonacci sequence:

```gleam
yielder.unfold(#(0, 1), fn(pair) {
  let x = pair.0 + pair.1
  yielder.Next(element: x, accumulator: #(pair.1, x))
})
|> yielder.take(6)
|> yielder.to_list
// -> [1, 2, 3, 5, 8, 13]
```

The sequence here is infinite, so the `take` function is used to make it finite before collecting the values into a list. If `to_list` is called on an infinite yielder the program will run until the program runs out of memory and crashes.
