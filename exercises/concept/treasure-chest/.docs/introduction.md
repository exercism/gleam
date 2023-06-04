# Introduction

## Generics

A type is a _generic type_ when it can contain values of any type using a _type parameter_.

For example, this `Box` type has the type parameter `a` (written with lowercase letters).

```gleam
pub type Box(a) {
  Box(item: a)
}
```

The `a` type parameter is a placeholder for any type, so `Box` can be used with strings, ints, or any other type.

```gleam
Box("Hello, Joe!") // The type is Box(String)

Box(42) // The type is Box(Int)
```

A type can have multiple type parameters by separating them with commas.

```gleam
pub type Pair(a, b) {
  Pair(first: a, second: b)
}
```

### Generic functions

Type parameters can also be used in the arguments of functions.

This function takes a value of the type `a` and calls twice a function that takes an `a` and returns a `b`.

```gleam
pub fn twice(value: a, f: fn(a) -> b) -> b {
  f(value)
  f(value)
}
```
