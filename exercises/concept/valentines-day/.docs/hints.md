# Hints

## 1. Define the approval

- Custom types can be defined using the `pub type Name { Variant1 }` syntax.

## 4. Define the activity

- Custom types containing data can be defined using the `pub type Name { Variant1(SomeType) }` syntax.

## 5. Rate the activity

- Custom type variants can be pattern matched on using case expressions.
- If you want to add an additional condition to a pattern, you can add a guard to the pattern using the `if` keyword.
