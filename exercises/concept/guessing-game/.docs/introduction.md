# Introduction

## Case Expressions

In Gleam a `case` expression can be used to conditionally execute code. With a case expression a value can be tested against one or more _patterns_. An example of such a pattern is the _literal pattern_, which matches a value against a literal value (e.g. `1` or `"hello"`).

Case expressions are written with the `case` keyword:

```gleam
pub fn describe(number: Int) -> String {
  case number {
    0 -> "Zero"
    1 -> "One"
  }
}
```

While this may look like `switch` statements in other languages, pattern matching starts to shine when also using other patterns. One such pattern is the _variable pattern_, which allows one to assign a value to a variable. In this example, the variable `i` will be assigned the value of `number` if it is not `0`:

```gleam
pub fn describe(number: Int) -> String {
  case number {
    0 -> "Zero"
    i -> "Non zero"
  }
}
```

In some cases, you may want to add an additional condition to a pattern. This is known as a _guard_ (clause), which can be added using the `if` keyword:

```gleam
pub fn describe(number: Int) -> String {
  case number {
    0 -> "Zero"
    i if i < 0 -> "Negative number"
  }
}
```

For performance reasons only basic mathematical and boolean operators are allowed in guards, other functions cannot be used in guards.

In the above example, not all possible input will have a matching pattern. The compiler will detect this and output an error. This is known as _exhaustive checking_. To solve the warning, one has to handle all cases. For this, the _discard pattern_ can be used, which is a pattern that matches on any value:

```gleam
pub fn describe(number: Int) -> String {
  case number {
    0 -> "Zero"
    i if i < 0 -> "Negative number"
    _ -> "Positive number"
  }
}

// No compiler error
```

Multiple patterns can be matched against in one clause using the `|` to separate between each pattern. Only one guard can be added to a case:

```gleam
pub fn describe(number: Int) -> String {
  let yell = True
  case number {
    0 -> "Zero"
    3 | 5 -> "One of the first odd prime numbers"
    2 | 4 | 6 | 8 if !yell -> "One of the even numbers less than 10"
    i if i % 2 == 0 && yell -> "[Yelling] I'm telling you! It's a even nunber"
    i if i < 0 -> "Negative number"
    _ -> "Positive number"
  }
}
// No compiler error
```

Case expressions will test a value against each pattern from top to bottom, until it finds a matching pattern and executes the logic associated with that pattern. The order of patterns matters!
