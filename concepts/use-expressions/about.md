# About

In Gleam it is common to write and use higher order functions, that is functions that take other functions as arguments. Sometimes when using many higher order functions at once the code can become difficult to read, with many layers of indentation.

For example, here is a function that calls several functions that return `Result(Int, Nil)`, and sums the values if all three are successful.

```gleam
import gleam/result

pub fn main() -> Result(Int, Nil) {
  result.try(function1(), fn(a) {
    result.try(function2(), fn(b) {
      result.try(function3(), fn(c) {
        result.try(function4(), fn(d) {
          Ok(a + b + c + d)
        })
      })
    })
  })
}
```

Gleam's `use` expressions allow us to write this code without the indentation, often making it easier to read.

```gleam
import gleam/result

pub fn main() -> Result(Int, Nil) {
  use a <- result.try(function1())
  use b <- result.try(function2())
  use c <- result.try(function3())
  use d <- result.try(function4())
  Ok(a + b + c + d)
}
```

A `use` expression collects all the following statements in the block into and passes it as a callback function as the final argument to the function call. The variables between the `use` keyword and the `<-` symbol are the names of the arguments that will be passed to the callback function.

```gleam
// This use expression
use a <- function(1, 2)
io.println("Hello!")
a

// Is equivalent to this normal function call
function(1, 2, fn(a) {
  io.println("Hello!")
  a
})
```

The callback function can take any number of arguments, or none at all.

```gleam
use a, b, c, d <- call_4_function()

use <- call_0_function()
```

There are no special requirements to create a function that can be called with a `use` expression, other than taking a callback function as the final argument.

```gleam
pub fn call_twice(function: fn() -> t) -> #(t, t) {
  let first = function()
  let second = function()
  #(first, second)
}
```

Gleam's `use` expressions are a very powerful feature that can be applied to lots of problems, but when overused they can make code difficult to read. It is generally preferred to use the normal function call syntax and only reach for `use` expressions when they make the code easier to read.
