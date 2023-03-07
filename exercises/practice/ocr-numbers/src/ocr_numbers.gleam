pub type Output {
  Unknown
  Digit(Int)
  List(List(Output))
}

pub type Error {
  InvalidLineNumber
  InvalidRowNumber
}

pub fn convert(input: String) -> Result(Output, Error) {
  todo
}
