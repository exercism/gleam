# Introduction

External types can be used to refer to data types defined in other languages, such as Erlang or JavaScript.

To define an external function declare a type but do not provide any constructors. This can then be used in the same way as any other type.

```rust
pub type OrderedDictionary(element)
```
