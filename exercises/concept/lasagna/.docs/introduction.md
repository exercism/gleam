# Introduction

## Basics

### Functions

In Gleam functions are defined using the `pub fn` syntax.

```gleam
pub fn add(x: Int, y: Int) -> Int {
  x + y
}
```

This function takes two arguments, both of type `Int`, and returns a value of type `Int`. There is no `return` keyword in Gleam, the value of the last expression in a function is always _implicitly returned_.

The type annotations for arguments and the return type are optional, and Gleam will always fully type check your code. Typically Gleam programmers will give their functions type annotations for clarity, but you may omit them if you wish.

```gleam
pub fn add(x, y) {
  x + y
}
```

A function can be called using the `function_name(argument1, argument2)` syntax.

```gleam
pub fn double(x: Int) -> Int {
  // Call the add function defined above
  add(x, x)
}
```

### Variables

In Gleam variables are defined using the `let name = expression` syntax.

```gleam
pub fn main() {
  let count = 1
}
```

Variables can be declared with type annotations. Like function arguments these are optional, though most Gleam programmers will omit type annotations for variables for brevity.

```gleam
pub fn main() {
  let count: Int = 1
}
```

### String literals

In Gleam strings are written using double quotes.

```gleam
pub fn greeting() {
  "Hello, world!"
}
```

### Code comments

Comments can be used to leave notes for other developers reading the source code. Comments in Gleam are single lines preceeded by `//`.

```gleam
pub fn main() {
  // This is a comment
  let x = 1
}
```
