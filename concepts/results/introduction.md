# Introduction

Gleam doesn't use exceptions for error handling, instead the generic `Result` type is returned by functions that can either succeed or fail.

The `Result` type is built into the language so you don't need to define or import it, but if you were to define it yourself it would look like this:

```gleam
pub type Result(value, error) {
  Ok(value)
  Error(error)
}
```

The `Ok` variant is returned when a function succeeds, and the `Error` variant is returned when a function fails.

Results are very common in Gleam, and Gleam programmers will commonly use the [`gleam/result` module](https://hexdocs.pm/gleam_stdlib/gleam/result.html) to make working with them easier.

The `result.map` function can be used to call a function on the value inside a result if it is an `Ok`, or to pass through an `Error` unchanged.

```gleam
Ok(1)
|> result.map(fn(x) { x + 1 })
// -> Ok(2)

Error("Oh no!")
|> result.map(fn(x) { x + 1 })
// -> Error("Oh no!")
```

The `result.try` function is similar, but the callback function is expected to return a result. This is useful for chaining together multiple functions that return results.

```gleam
Ok(1)
|> result.try(fn(x) { Ok(x + 1) })
// -> Ok(2)

Ok(1)
|> result.try(fn(x) { Error("Nope!") })
// -> Error("Nope!")
```
