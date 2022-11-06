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
  todo("Implement this function")
}

pub fn format_stack(f: Forth) -> String {
  todo("Implement this function")
}

pub fn eval(f: Forth, prog: String) -> Result(Forth, ForthError) {
  todo("Implement this function")
}
