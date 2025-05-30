import gleam/deque.{type Deque}

pub opaque type CircularBuffer(t) {
  CircularBuffer(capacity: Int, size: Int, queue: Deque(t))
}

pub fn new(capacity: Int) -> CircularBuffer(t) {
  let assert True = capacity > 0
  CircularBuffer(capacity, 0, deque.new())
}

pub fn read(buffer: CircularBuffer(t)) -> Result(#(t, CircularBuffer(t)), Nil) {
  case deque.pop_front(buffer.queue) {
    Ok(#(item, queue)) -> {
      let buffer = CircularBuffer(..buffer, queue: queue, size: buffer.size - 1)
      Ok(#(item, buffer))
    }
    Error(Nil) -> Error(Nil)
  }
}

pub fn write(
  buffer: CircularBuffer(t),
  item: t,
) -> Result(CircularBuffer(t), Nil) {
  case buffer.size < buffer.capacity {
    True -> Ok(unchecked_write(buffer, item))
    False -> Error(Nil)
  }
}

pub fn overwrite(buffer: CircularBuffer(t), item: t) -> CircularBuffer(t) {
  case buffer.size < buffer.capacity {
    True -> unchecked_write(buffer, item)
    False ->
      buffer
      |> discard_oldest
      |> unchecked_write(item)
  }
}

fn unchecked_write(buffer: CircularBuffer(t), item: t) -> CircularBuffer(t) {
  let queue = deque.push_back(buffer.queue, item)
  CircularBuffer(..buffer, queue: queue, size: buffer.size + 1)
}

fn discard_oldest(buffer: CircularBuffer(t)) -> CircularBuffer(t) {
  let queue = case deque.pop_front(buffer.queue) {
    Ok(#(_, queue)) -> queue
    Error(_) -> deque.new()
  }
  CircularBuffer(..buffer, queue: queue, size: buffer.size - 1)
}

pub fn clear(buffer: CircularBuffer(t)) -> CircularBuffer(t) {
  new(buffer.capacity)
}
