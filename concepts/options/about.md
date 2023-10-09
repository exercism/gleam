# About

The `Option` type is used to represent values that can either be absent or present.

It is defined in the `gleam/option` module as follows:

```gleam
type Option(a) {
  Some(a)
  None
}
```

The `Some` constructor is used to wrap a value when it's present, and the `None` constructor is used to represent the absence of a value.

Accessing the content of a `Option` is often done via pattern matching.

```gleam
import gleam/option.{Option, Some, None}

pub fn say_hello(person: Option(String)) -> String {
  case person {
    Some(name) -> "Hello, " <> name <> "!"
    None -> "Hello, Friend!"
  }
}
```

```gleam
say_hello(Some("Matthieu"))
// -> "Hello, Matthieu!"

say_hello(None)
// -> "Hello, Friend!"
```

The `gleam/option` module also defines a number of useful function for working with `Option` types, such as `unwrap`, which returns the content of an `Option` or a default value if it is `None`.

```gleam
import gleam/option.{Option}

pub fn say_hello_again(person: Option(String)) -> String {
  let name = option.unwrap(person, "Friend")
  "Hello, " <> name <> "!"
}
```

