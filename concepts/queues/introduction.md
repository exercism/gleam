# Introduction

The Gleam standard library implements a `Queue` type in the `gleam/queue` module. It is similar to the `List` type, but with a few key differences:

- The `Queue` type doesn't have a literal syntax that can be used to construct queues or pattern match on them.
- Elements can be efficiently added to and removed from both the front and back of the `Queue` type, while the `List` type can only efficiently add and remove elements from the front.

Most of the time you will want to use the `List` type, but when you need to be able to add and remove from the end of a collection, the `Queue` type is a good choice.

Queues can be created using the `queue.new` and `queue.from_list` functions:

```gleam
let empty = queue.new()
let one_to_four = queue.from_list([1, 2, 3, 4])
```

Elements can be added to the queue using the `queue.push_front` and `queue.push_back` functions:

```gleam
let one_to_four = queue.from_list([1, 2, 3, 4])
let one_to_five = queue.push_back(one_to_four, 5)
let zero_to_five = queue.push_front(one_to_five, 0)
```

Elements can be removed from the queue using the `queue.pop_front` and `queue.pop_back` functions:

```gleam
let one_to_four = queue.from_list([1, 2, 3, 4])

queue.pop_back(one_to_four)
// -> Ok(#(4, queue.from_list([1, 2, 3])))

queue.pop_front(one_to_four)
// -> Ok(#(1, queue.from_list([2, 3, 4])))

queue.pop_front(queue.new())
// -> Error(Nil)
```

A queue can be converted into a list using the `queue.to_list` function:

```gleam
let one_to_four = queue.from_list([1, 2, 3, 4])
queue.to_list(one_to_four)
// -> [1, 2, 3, 4]
```

## Queue equality

Due to how queues are implemented, two queues with the same elements in the same order may not be equal according to the `==` operator, which compares values structurally. For example:

```gleam
let empty = queue.new()
let a = queue.push_front(empty, 1)
let b = queue.push_back(empty, 1)
a == b
// -> False
```

If you need to compare two queues for equality you can use the `queue.is_logically_equal` function.

```gleam
queue.is_logically_equal(a, b, fn(x, y) { x == y })))
// -> True
```
