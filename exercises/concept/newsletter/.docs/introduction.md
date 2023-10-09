# Introduction

## Nil

`Nil` in Gleam is a type with a single value, also called `Nil`. It is similar to `void` in other languages in that it is used when a function does not have any more suitable value to return.

```gleam
io.println("Hello, Joe!")
// -> Nil
```

Values in Gleam are not "nil-able" or "nullable" like in some other languages. A value can only be `Nil` if it's type is `Nil`, and a value of any other type can never be `Nil`.

## IO

Like most programming language Gleam has "side effects", so functions can read and change the state of the world, as well as returning a value.

The `gleam/io` module in the Gleam standard library provides functions for printing strings to the console.

```gleam
io.println("Hello, Joe!")
// Hello, Joe!
// -> Nil
```

Other packages may provide other IO functions, such as `simplifile`, a package which provides functions for reading and writing files.

```gleam
simplifile.read("favourite-colour.txt")
// -> Ok("Pink\n")
```
