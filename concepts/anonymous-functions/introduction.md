# Introduction

Gleam has first class functions, meaning functions are normal values that can be assigned to variables, passed as arguments, and returned from other functions.

A named function defined in a module can be referenced by its name.

```gleam
pub fn main() {
  // Assign the function to a variable
  let f = add_one
  
  // Invoke the function
  f(1) // -> 2
  f(2) // -> 3
}

fn add_one(x) {
  x + 1
}
```

Gleam also has anonymous functions, which are defined within other functions.

```gleam
// Define the function
let f = fn(x) { x + 1 }

// Invoke the function
f(1) // -> 2
f(2) // -> 3
```

Anonymous functions can reference variables that were in scope when they were defined, making them _closures_.

```gleam
let secret_number = 42

// This function always returns 42
fn() { secret_number }
```

The _function capture_ syntax provides a convenient shorthand for creating anonymous functions that pass a single argument to a function. These two expressions are equivalent:

```gleam
// Anonymous function syntax
let f = fn(x) { my_function(1, 2, x) }

// Function capture syntax
let f = my_function(1, 2, _)
```
