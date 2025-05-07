import gleam/deque.{type Deque}

pub fn insert_top(deque: Deque(Int), card: Int) {
  deque.push_back(deque, card)
}

pub fn remove_top_card(deque: Deque(Int)) -> Deque(Int) {
  case deque.pop_back(deque) {
    Ok(#(_, new_deque)) -> new_deque
    Error(_) -> deque
  }
}

pub fn insert_bottom(deque: Deque(Int), card: Int) -> Deque(Int) {
  deque.push_front(deque, card)
}

pub fn remove_bottom_card(deque: Deque(Int)) -> Deque(Int) {
  case deque.pop_front(deque) {
    Ok(#(_, new_deque)) -> new_deque
    Error(_) -> deque
  }
}

pub fn check_size_of_stack(deque: Deque(Int), target: Int) -> Bool {
  deque.length(deque) == target
}
