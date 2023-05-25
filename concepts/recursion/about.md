# About

The ability for something to be defined in terms of itself is called recursion. In Gleam, recursion is most commonly found in recursive functions, which are functions that call themselves.

A recursive function needs to have at least one _base case_ and at least one _recursive case_. A _base case_ returns a value without calling the function again. A _recursive case_ calls the function again, modifying the input so that it will at some point match the base case.

```gleam
pub fn factorial(x: Int) -> Int {
  case x {
    // Base case
    1 -> 1

    // Recursive case
    _ -> x * factorial(x - 1)
  }
}
```

Gleam has no special syntax for looping, so all looping is done with recursion.

```gleam
pub fn list_length(list: List(String)) -> Int {
  case list {
    [] -> 0
    [_, ..rest] -> 1 + list_length(rest)
  }
}
```

Gleam also supports recursive custom types. A recursive custom type has one or more of its variants refer to itself in their contained data.

```gleam
pub type RussianDoll {
  Child               // Base case
  Mother(RussianDoll) // Recursive case
}
```
```gleam
let very_big_doll = Mother(Mother(Mother(Child)))
let small_doll = Mother(Child)
```

