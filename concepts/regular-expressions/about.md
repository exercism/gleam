# About

Regular expressions in Gleam follow the **PCRE** specification (**P**erl **C**ompatible **R**egular **E**xpressions), similarly to other popular languages like Java, JavaScript, or Ruby.

The `gleam/regex` module offers functions for working with regular expressions.

~~~~exercism/note
This exercise assumes that you already know regular expression syntax, including character classes, quantifiers, groups, and captures.

if you need to refresh your regular expression knowledge, check out one of those sources: [Regular-Expressions.info](https://www.regular-expressions.info), [Rex Egg](https://www.rexegg.com/), [RegexOne](https://regexone.com/), [Regular Expressions 101](https://regex101.com/), [RegExr](https://regexr.com/).
~~~~

The most common way to create regular expressions is using the `regex.from_string` function.

```gleam
let assert Ok(re) = regex.from_string("test")
```

The `regex.check` function can be used to check if a regular expression matches a string.

```gleam
let assert Ok(re) = regex.from_string("test")

regex.check(re, "this is a test")
// -> True

regex.check(re, "this is too")
// -> False
```


## Captures

If you wish to capture substrings using a regular expression the `regex.scan` function can be used to return a list of matches.

```gleam
let assert Ok(re) = regex.from_string("[oi]n a (\\w+)")
regex.scan(with: re, content: "I am on a boat in a lake.")
// -> [
//   Match(
//     content: "on a boat",
//     submatches: [Some("boat")]
//   ),
//   Match(
//     content: "in a lake",
//     submatches: [Some("lake")]
//   )
// ]
```

## Modifiers

The behaviour of a regular expression can be modified by creating it with the `regex.compile` function and passing in options.

```gleam
let options = regex.Options(case_insensitive: True, multi_line: False)
let assert Ok(re) = regex.compile("[A-Z]", with: options)
regex.check(re, "abc123")
// -> True
```
