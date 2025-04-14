# Introduction

## Regular Expressions

Regular expressions in Gleam follow the **PCRE** specification (**P**erl **C**ompatible **R**egular **E**xpressions), similarly to other popular languages like Java, JavaScript, or Ruby.

The `gleam/regexp` module offers functions for working with regular expressions.

~~~exercism/note
This exercise assumes that you already know regular expression syntax, including character classes, quantifiers, groups, and captures.

if you need to refresh your regular expression knowledge, check out one of those sources: [Regular-Expressions.info](https://www.regular-expressions.info), [Rex Egg](https://www.rexegg.com/), [RegexOne](https://regexone.com/), [Regular Expressions 101](https://regex101.com/), [RegExr](https://regexr.com/).
~~~

The most common way to create regular expressions is using the `regexp.from_string` function.

```gleam
let assert Ok(re) = regexp.from_string("test")
```

The regular expression creation functions return an error if the regular expression syntax is invalid, so a let-assertion has been used here to ensure the regular expression is valid.

The `regexp.check` function can be used to check if a regular expression matches a string.

```gleam
let assert Ok(re) = regexp.from_string("test")

regexp.check(re, "this is a test")
// -> True

regexp.check(re, "this is too")
// -> False
```

### Captures

If you wish to capture substrings using a regular expression the `regexp.scan` function can be used to return a list of matches.

```gleam
let assert Ok(re) = regexp.from_string("[oi]n a (\\w+)")
regexp.scan(with: re, content: "I am on a boat in a lake.")
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

### Modifiers

The behaviour of a regular expression can be modified by creating it with the `regexp.compile` function and passing in options.

```gleam
let options = regexp.Options(case_insensitive: True, multi_line: False)
let assert Ok(re) = regexp.compile("[A-Z]", with: options)
regexp.check(re, "abc123")
// -> True
```
