pub opaque type CircularBuffer(t) {
  CircularBuffer
}

pub fn new(capacity: Int) -> CircularBuffer(t) {
  todo
}

pub fn read(buffer: CircularBuffer(t)) -> Result(#(t, CircularBuffer(t)), Nil) {
  todo
}

pub fn write(
  buffer: CircularBuffer(t),
  item: t,
) -> Result(CircularBuffer(t), Nil) {
  todo
}

pub fn overwrite(buffer: CircularBuffer(t), item: t) -> CircularBuffer(t) {
  todo
}

pub fn clear(buffer: CircularBuffer(t)) -> CircularBuffer(t) {
  todo
}
