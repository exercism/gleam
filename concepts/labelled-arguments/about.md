# About

Sometimes when a function takes many arguments it can be difficult to remember the order they should be given in. To aid readability Gleam allows arguments to be labelled with a name.

To label an argument specify the label name before the argument name, separated by a space.

```gleam
pub fn replace(
  in string: String,
  each target: String,
  with replacement: String
) -> String {
  // ...
}
```

When calling a function with labelled arguments the arguments can be given in any order. Each of these calls to `replace` are equivalent:

```gleam
replace(in: "🍔🍔🍔" each: "🍔", with: "🍕")

replace(each: "🍔", in: "🍔🍔🍔", with: "🍕")

replace(with: "🍕", each: "🍔", in: "🍔🍔🍔")
```

Labels are always optional. Even if a function has labelled arguments they can still be called without labels.

```gleam
replace("🍔🍔🍔", "🍔", "🍕")
```

When defining a function with labelled arguments all arguments after the first labelled argument must also be labelled.

```gleam
// This function is valid
pub fn valid(
  x: Int,
  label1 y: Int,
  label2 z: Int
) -> Int {
  // ...
}

// This function is invalid
pub invalid(
  x: Int,
  label1 y: Int,
  z: Int
) -> Int {
  // ...
}
```
