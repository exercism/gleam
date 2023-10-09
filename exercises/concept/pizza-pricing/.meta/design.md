# Design

## Learning objectives

- Know about stack frames and that they use memory.
- Know about tail call optimisation.
- Know about accumulator arguments.

## Out of scope

- Mutual recursion.
- Stack vs heap allocated stack frames.
- Differences between Erlang and JavaScript optimisation.

## Concepts

- `tail-call-optimisation`

## Prerequisites

- `recursion`
- `pattern-matching`
- `lists`
- `ints`

## Analyzer

- Check that no imports are used.
- Check that the functions are tail recursive.
- Check that list.length is not used for the special cases of 1 or 2 pizzas in `order_price`.
