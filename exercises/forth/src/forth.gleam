import gleam/result

pub type Forth {
  Forth
}

pub type ForthError {
  DivisionByZero
  StackUnderflow
  InvalidWord
  UnknownWord
}

pub fn new() -> Forth {
  Forth
}

pub fn format_stack(f: Forth) -> String {
  "I'm a little stack, short and stout..."
}

pub fn eval(f: Forth, prog: String) -> Result(Forth, ForthError) {
  Ok(f)
}
