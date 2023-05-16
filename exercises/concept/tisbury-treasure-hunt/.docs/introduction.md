# Introduction

## Tuples

A tuple is an ordered container of values. Like all Gleam data types tuples are immutable.
Each element of a tuple can be of a different type -- they can even be other tuples.

Tuples are defined as comma-separated values between `#(` and `)`: `#(1, 2.0, "Three")`.

```gleam
#("one", 2)      // Tuple pair (2 values)
#("one", 2, 3.0) // Tuple triplet (3 values)
```

Tuples with the same length and the same types (in the same order) can be compared for equality.

```gleam
#(1, 2) == #(1, 2) // Same length, same types, same values, same order
// -> True

#(1, 2) == #(2, 1) // Same length, same types, same values, different order
// -> False

#(1, 2) == #(1, "2") // Same length, different types
// Compile error!

#(1, 2) == #(1, 2, 3) // Different length
// Compile error!
```

There are three ways in which you can get the contained values out of a tuple:

- Indexing.
- Pattern matching with `let`.
- Pattern matching with `case`.

```gleam
let person = #("Jordan", 170)

// Option 1: Indexing
person.0 // -> "Jordan"
person.1 // -> 170

// Option 2: let
let #(name2, length2) = person
// -> name2 = "Jordan"
// -> length2 = 52

// Option 3: case
case person {
  #(name3, length3) -> {
    name3   // -> "Jordan"
    length3 // -> 170
  }
}
```
