# Hints

## 1. Insert a card at the of top the stack

- The [`deque.push_back` function][push-back] can be used to add to the back of a deque.

## 2. Remove the top card from the stack

- The [`deque.pop_back` function][pop-back] can be used to remove from the back of a deque.

## 3. Insert a card at the bottom of the stack

- The [`deque.push_front` function][push-front] can be used to add to the front of a deque.

## 4. Remove a card from the bottom of the stack

- The [`deque.pop_front` function][pop-front] can be used to remove from the front of a deque.

## 5. Check equality

- The [deque.is_logically_equal][is_logically_equal] function must be used to check for equality.
  As two deques with the same content might be structural different, the normal equality operator `==` should not be used.

[push-back]: https://hexdocs.pm/gleam_deque/gleam/deque.html#push_back
[pop-back]: https://hexdocs.pm/gleam_deque/gleam/deque.html#pop_back
[push-front]: https://hexdocs.pm/gleam_deque/gleam/deque.html#push_front
[pop-front]: https://hexdocs.pm/gleam_deque/gleam/deque.html#pop_front
[is_logically_equal]: https://hexdocs.pm/gleam_deque/gleam/deque.html#is_logically_equal
