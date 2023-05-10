import gleeunit
import gleeunit/should
import ocr_numbers.{Digit, InvalidLineNumber, InvalidRowNumber, List, Unknown}

pub fn main() {
  gleeunit.main()
}

pub fn recognizes_0_test() {
  ocr_numbers.convert(
    "
 _ 
| |
|_|
   ",
  )
  |> should.equal(Ok(Digit(0)))
}

pub fn recognizes_1_test() {
  ocr_numbers.convert(
    "
   
  |
  |
   ",
  )
  |> should.equal(Ok(Digit(1)))
}

pub fn unreadable_but_correctly_sized_inputs_return_question_mark_test() {
  ocr_numbers.convert(
    "
   
  _
  |
   ",
  )
  |> should.equal(Ok(Unknown))
}

pub fn input_with_a_number_of_lines_that_is_not_a_multiple_of_four_raises_an_error_test() {
  ocr_numbers.convert(
    "
 _ 
| |
   ",
  )
  |> should.equal(Error(InvalidLineNumber))
}

pub fn input_with_a_number_of_columns_that_is_not_a_multiple_of_three_raises_an_error_test() {
  ocr_numbers.convert(
    "
    
   |
   |
    ",
  )
  |> should.equal(Error(InvalidRowNumber))
}

pub fn recognizes_110101100_test() {
  ocr_numbers.convert(
    "
       _     _        _  _ 
  |  || |  || |  |  || || |
  |  ||_|  ||_|  |  ||_||_|
                           ",
  )
  |> should.equal(Ok(List([
    Digit(1),
    Digit(1),
    Digit(0),
    Digit(1),
    Digit(0),
    Digit(1),
    Digit(1),
    Digit(0),
    Digit(0),
  ])))
}

pub fn garbled_numbers_in_a_string_are_replaced_with_test() {
  ocr_numbers.convert(
    "
       _     _           _ 
  |  || |  || |     || || |
  |  | _|  ||_|  |  ||_||_|
                           ",
  )
  |> should.equal(Ok(List([
    Digit(1),
    Digit(1),
    Unknown,
    Digit(1),
    Digit(0),
    Unknown,
    Digit(1),
    Unknown,
    Digit(0),
  ])))
}

pub fn recognizes_2_test() {
  ocr_numbers.convert(
    "
 _ 
 _|
|_ 
   ",
  )
  |> should.equal(Ok(Digit(2)))
}

pub fn recognizes_3_test() {
  ocr_numbers.convert(
    "
 _ 
 _|
 _|
   ",
  )
  |> should.equal(Ok(Digit(3)))
}

pub fn recognizes_4_test() {
  ocr_numbers.convert(
    "
   
|_|
  |
   ",
  )
  |> should.equal(Ok(Digit(4)))
}

pub fn recognizes_5_test() {
  ocr_numbers.convert(
    "
 _ 
|_ 
 _|
   ",
  )
  |> should.equal(Ok(Digit(5)))
}

pub fn recognizes_6_test() {
  ocr_numbers.convert(
    "
 _ 
|_ 
|_|
   ",
  )
  |> should.equal(Ok(Digit(6)))
}

pub fn recognizes_7_test() {
  ocr_numbers.convert(
    "
 _ 
  |
  |
   ",
  )
  |> should.equal(Ok(Digit(7)))
}

pub fn recognizes_8_test() {
  ocr_numbers.convert(
    "
 _ 
|_|
|_|
   ",
  )
  |> should.equal(Ok(Digit(8)))
}

pub fn recognizes_9_test() {
  ocr_numbers.convert(
    "
 _ 
|_|
 _|
   ",
  )
  |> should.equal(Ok(Digit(9)))
}

pub fn recognizes_string_of_decimal_numbers_test() {
  ocr_numbers.convert(
    "
    _  _     _  _  _  _  _  _ 
  | _| _||_||_ |_   ||_||_|| |
  ||_  _|  | _||_|  ||_| _||_|
                              ",
  )
  |> should.equal(Ok(List([
    Digit(1),
    Digit(2),
    Digit(3),
    Digit(4),
    Digit(5),
    Digit(6),
    Digit(7),
    Digit(8),
    Digit(9),
    Digit(0),
  ])))
}

pub fn numbers_separated_by_empty_lines_are_recognized_lines_are_joined_by_commas_test() {
  ocr_numbers.convert(
    "
    _  _ 
  | _| _|
  ||_  _|
         
    _  _ 
|_||_ |_ 
  | _||_|
         
 _  _  _ 
  ||_||_|
  ||_| _|
         ",
  )
  |> should.equal(Ok(List([
    List([Digit(1), Digit(2), Digit(3)]),
    List([Digit(4), Digit(5), Digit(6)]),
    List([Digit(7), Digit(8), Digit(9)]),
  ])))
}
