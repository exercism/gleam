# Introduction

## Bools

Gleam represents true and false values with the `Bool` type. There are only two values: `True` and `False`. These values can be bound to a variable:

```gleam
let true_variable = True
let false_variable = False
```

The `&&`, `||`, and `!` operators can be used to manipulate boolean values:

```gleam
True && True  // -> True
True && False // -> False

False || True  // -> True
False || False // -> False

!False // -> True
!True  // -> False
```

The `&&` and `||` operators use short-circuit evaluation, which means that the expression on the right-hand side of the operator is only evaluated if needed.

Each of the operators has a different precedence, where `!` is evaluated before `&&` and `||`. Braces can be used to evaluate one part of the expression before the others:

```gleam
!True && False   // -> False
!{True && False} // -> True
```
