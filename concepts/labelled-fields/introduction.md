# Introduction

When a custom type variant holds data it is called a record, and each contained value resides in a _field_.

```gleam
pub type Rectangle {
  Rectangle(
    Float, // The first field
    Float, // The second field
  )
}
```

To aid readability Gleam allows fields to be labelled with a name.

```gleam
pub type Rectangle {
  Rectangle(
    width: Float,
    height: Float,
  )
}
```

Labels can be used to give arguments in any order to the constructor of a record.

```gleam
let a = Rectangle(height: 10.0, width: 20.0)
let b = Rectangle(width: 20.0, height: 10.0)

a == b
// -> True
```

When a custom type has only one variant then the `.label` accessor syntax can be used to get the fields of a record.

```gleam
let rect = Rectangle(height: 10.0, width: 20.0)

rect.height // -> 10.0
rect.width  // -> 20.0
```

The record update syntax can be used when a custom type has a single variant to create a new record from an existing one, but with some of the fields replaced with new values.

```gleam
let rect = Rectangle(height: 10.0, width: 20.0)
let tall_rect = Rectangle(..rect, height: 50.0)

tall_rect.height // -> 50.0
tall_rect.width  // -> 20.0
```

Labels can also be used when pattern matching to extract values from records.

```gleam
pub fn is_tall(rect: Rectangle) {
  case rect {
    Rectangle(height: h, width: _) if h > 20.0 -> True
    _ -> False
  }
}
```

If we only want to match on some of the fields we can use the spread operator `..` to ignore any remaining fields.

```gleam
pub fn is_tall(rect: Rectangle) {
  case rect {
    Rectangle(height: h, ..) if h > 20.0 -> True
    _ -> False
  }
}
```
