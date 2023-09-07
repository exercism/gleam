# Instructions

As a magician-to-be, Elyse needs to practice some basics. She has a stack of cards that she wants to manipulate.

To make things a bit easier she only uses the cards 1 to 10.

## 1. Insert a card at the of top the stack

Implement the function `insert_top` that returns a copy of the stack with the new card provided added to the top of the stack.

```gleam
insert_top(queue.from_list([5, 9, 7, 1]), 8)
// -> queue.from_list([5, 9, 7, 1, 8])
```

## 2. Remove the top card from the stack

Implement the function `remove_top_card` that returns a copy of the stack which has had the card at the top of the stack removed. If the given stack is empty, the original stack should be returned, unchanged.

```gleam
remove_top_card(queue.from_list([3, 2, 6, 4, 8]))
// -> queue.from_list([3, 2, 6, 4])
```

## 3. Insert a card at the bottom of the stack

Implement the function `insert_bottom` that returns a copy of the stack with the new card provided added to the bottom of the stack.

```gleam
insert_bottom(queue.from_list([5, 9, 7, 1]), 8)
// -> queue.from_list([8, 5, 9, 7, 1])
```

## 4. Remove a card from the bottom of the stack

Implement the function `remove_bottom_card` that returns a copy of the stack which has had the card at the bottom of the stack removed. If the given stack is empty, the original stack should be returned, unchanged.

```gleam
remove_bottom_card(queue.from_list([8, 5, 9, 7, 1]))
// -> queue.from_list([5, 9, 7, 1])
```

## 5. Check size of the stack

Implement the function `check_size_of_stack` that checks whether the size of the stack is equal to a given `stack_size` or not.

```gleam
check_size_of_stack(queue.from_list([3, 2, 6, 4, 8]), 4)
// -> False
```
