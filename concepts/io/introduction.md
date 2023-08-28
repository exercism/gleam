# Introduction

Like most programming language Gleam has "side effects", so functions can read with and change the state of the world, as well as returning a value.

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
