# Design

## Learning objectives

After completing this exercise, the student should:

- Know about the pipe operator.
- Know the two different ways the pipe operator can call functions.
- Know how to use a `_` placeholder to pipe into other positions.

## Out of scope

- Function captures (the `_` syntax).

## Prerequisites

- `strings`: Manipulating strings.
- `modules`: Importing modules.
- `results`: The string functions return `Result`s.
- `lists`: The string functions return `List`s.

## Concepts

- `pipe-operator`

## Analyzer

This exercise could benefit from the following rules added to the the analyzer:

- Verify that the `initial` function calls the `first_letter` function.
- Verify that the `initials` function calls the `initial` function.
- Verify that the `pair` function calls the `initials` function.
