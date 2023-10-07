# Introduction

Custom Types are how new data types are defined in Gleam. They can have multiple _variants_, each with their own name.

```gleam
pub type Season {
  Spring
  Summer
  Autumn
  Winter
}
```

Each case of a custom type can optionally hold some data, and different cases can have different types of data. When a variant holds data it is called a _record_.

```gleam
pub type Number {
  SomeInt(Int)
  SomeFloat(Float)
  Invalid
}
```

Creating a value for a specific case can be done by referring to its name if it contains no additional data (`Spring`), or by calling it as a function if it does (`SomeInt(2)`).

```gleam
let spring = Spring
let integerTwo = SomeInt(2)
```

Custom types, along with everything in Gleam, have _structural equality_, which means that two values of the same variant and with the same data are equivalent.

```gleam
Spring == Spring // -> True
Spring == Autumn // -> False
SomeInt(2) == SomeInt(2) // -> True
SomeInt(2) == SomeFloat(2.0) // -> False
```

Custom type variants can be pattern matched on using case expressions.

```gleam
import gleam/int
import gleam/float

pub fn describe(flexible_number: Number) -> String {
  case flexible_number {
    SomeFloat(f) -> "Float: " <> float.to_string(f)
    SomeInt(i) -> "Int: " <> int.to_string(i)
    Invalid -> "Neither a float nor an int"
  }
}
```
