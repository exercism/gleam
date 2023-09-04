# Introduction

## Opaque Types

Opaque types in Gleam are custom types where only the module that defines the type can construct or pattern match values of the type. This is useful for creating types that should only be used in a specific way.

Opaque types are defined using the `opaque` keyword.

```gleam
pub opaque type PositiveInt {
  PositiveInt(inner: Int)
}
```

This `PositiveInt` type is to be used in situations where an int is wanted, but it has to be zero or greater. A regular Gleam `Int` could not be used as they can also be negative.

The module that defines this type can define a function to get the inner int value, and a function for creating the `PositiveInt` type which will return an error if the value is not positive.

```gleam
pub fn from_int(i: Int) -> Result(PositiveInt, String) {
  case i {
    _ if i < 0 -> Error("Value must be positive")
    _ -> Ok(PositiveInt(i))
  }
}

pub fn to_int(i: PositiveInt) -> Int {
  i.inner
}
```

With this API other modules cannot construct a `PositiveInt` with a negative value, so any function that takes a `PositiveInt` can be sure that the value is positive.
