import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Error {
  SyntaxError
  UnknownOperation
  ImpossibleOperation
}

type Token {
  Operand(value: Int)
  Operator(fun: MathOperation)
}

type MathOperation =
  fn(Int, Int) -> Result(Int, Nil)

pub fn answer(question: String) -> Result(Int, Error) {
  question
  |> sanitize()
  |> result.then(tokenize)
  |> result.then(evaluate)
}

fn sanitize(question: String) -> Result(String, Error) {
  case
    question
    |> string.replace("What is", "")
    |> string.replace("by", "")
    |> string.replace("?", "")
  {
    "" -> Error(SyntaxError)
    question -> Ok(question)
  }
}

fn tokenize(question: String) -> Result(List(Token), Error) {
  question
  |> string.split(" ")
  |> list.filter(fn(chunk) { !string.is_empty(chunk) })
  |> parse_tokens()
  |> result.then(ensure_number_of_tokens_is_correct)
}

fn evaluate(tokens: List(Token)) -> Result(Int, Error) {
  try [Operand(result)] =
    list.try_fold(
      tokens,
      [],
      fn(previous_tokens: List(Token), current_token: Token) {
        case previous_tokens, current_token {
          [], Operand(_) -> Ok([current_token])

          [Operand(_)], Operator(_) -> Ok([current_token, ..previous_tokens])

          [Operator(operation), Operand(left_operand)], Operand(right_operand) ->
            case operation(left_operand, right_operand) {
              Ok(accumulated_result) -> Ok([Operand(accumulated_result)])
              Error(Nil) -> Error(ImpossibleOperation)
            }

          _, _ -> Error(SyntaxError)
        }
      },
    )

  Ok(result)
}

fn parse_tokens(chunks: List(String)) -> Result(List(Token), Error) {
  chunks
  |> list.try_fold(
    [],
    fn(tokens: List(Token), chunk: String) {
      case parse_token(chunk) {
        Ok(token) -> Ok([token, ..tokens])
        Error(error) -> Error(error)
      }
    },
  )
  |> result.map(list.reverse)
}

fn parse_token(chunk: String) -> Result(Token, Error) {
  case int.parse(chunk), parse_operation(chunk) {
    Ok(number), _ -> Ok(Operand(number))
    _, Ok(operation) -> Ok(Operator(operation))
    _, _ -> Error(UnknownOperation)
  }
}

fn parse_operation(chunk: String) -> Result(MathOperation, Nil) {
  case chunk {
    "plus" -> Ok(result_wrapped_operation(int.add))
    "minus" -> Ok(result_wrapped_operation(int.subtract))
    "multiplied" -> Ok(result_wrapped_operation(int.multiply))
    "divided" -> Ok(int.divide)
    _ -> Error(Nil)
  }
}

fn result_wrapped_operation(fun: fn(Int, Int) -> Int) -> MathOperation {
  fn(a, b) { Ok(fun(a, b)) }
}

fn ensure_number_of_tokens_is_correct(
  tokens: List(Token),
) -> Result(List(Token), Error) {
  let #(operands, operators) = list.partition(tokens, is_operand_token)

  let number_of_operands = list.length(operands)
  let number_of_operators = list.length(operators)

  case number_of_operands == number_of_operators + 1 {
    True -> Ok(tokens)
    False -> Error(SyntaxError)
  }
}

fn is_operand_token(token: Token) -> Bool {
  case token {
    Operand(_) -> True
    _ -> False
  }
}
