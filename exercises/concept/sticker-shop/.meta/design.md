# Design

## Learning objectives

- Know what a phantom type is.
- Know how to use phantom types to restrict functions.
- Know how to use phantom types with opaque types.

## Out of scope


## Concepts

- `phantom-types`

## Prerequisites

- `opaque-types`

## Analyzer

This exercise could benefit from the following rules added to the [analyzer][analyzer]:

- Verify that the type parameter is a phantom type and not stored in the record.
- Verify that the `total` function returns `Money` of the same currency as the list.

[analyzer]: https://github.com/exercism/gleam-analyzer
