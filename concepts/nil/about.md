# About

`Nil` in Gleam is a type with a single value, also called `Nil`. It is similar to `void` in other languages in that it is used when a function does not have any more suitable value to return.

```gleam
io.println("Hello, Joe!")
// -> Nil
```

Values in Gleam are not "nil-able" or "nullable" like in some other languages. A value can only be `Nil` if it's type is `Nil`, and a value of any other type can never be `Nil`.
