# Forth

Implement an evaluator for a very simple subset of Forth.

[Forth](https://en.wikipedia.org/wiki/Forth_%28programming_language%29)
is a stack-based programming language. Implement a very basic evaluator
for a small subset of Forth.

Your evaluator has to support the following words:

- `+`, `-`, `*`, `/` (integer arithmetic)
- `DUP`, `DROP`, `SWAP`, `OVER` (stack manipulation)

Your evaluator also has to support defining new words using the
customary syntax: `: word-name definition ;`.

To keep things simple the only data type you need to support is signed
integers of at least 16 bits size.

You should use the following rules for the syntax: a number is a
sequence of one or more (ASCII) digits, a word is a sequence of one or
more letters, digits, symbols or punctuation that is not a number.
(Forth probably uses slightly different rules, but this is close
enough.)

Words are case-insensitive.

## Running tests

Gleam projects are tested using `rebar3`, the standard Erlang build tool. 
Please refer to [the installation
instructions](http://exercism.io/languages/gleam/installation) for more information.

In order to run the tests, you can issue the following command from the exercise
directory.

```bash
$ rebar3 eunit
```

## Questions?

For detailed information about the Gleam track, please refer to the
[help page](http://exercism.io/languages/gleam) on the Exercism site.
This covers the basic information on setting up the development
environment expected by the exercises.


## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
