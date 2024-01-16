import gleam/result
import gleam/int
import gleam/dict
import gleam/order
import gleam/list
import gleam/string
import gleam/string_builder

pub type ForthWordDef {
  UserDef(List(ForthTok))
  BuiltIn(String)
}

pub type ForthTok {
  Value(Int)
  Word(String)
  WordDef(String, ForthWordDef)
}

pub type Forth {
  Forth(stack: List(Int), env: dict.Dict(String, ForthWordDef))
}

pub fn stdlib() -> dict.Dict(String, ForthWordDef) {
  dict.new()
  |> dict.insert("+", BuiltIn("+"))
  |> dict.insert("-", BuiltIn("-"))
  |> dict.insert("/", BuiltIn("/"))
  |> dict.insert("*", BuiltIn("*"))
  |> dict.insert("DUP", BuiltIn("DUP"))
  |> dict.insert("DROP", BuiltIn("DROP"))
  |> dict.insert("SWAP", BuiltIn("SWAP"))
  |> dict.insert("OVER", BuiltIn("OVER"))
}

pub type ForthError {
  DivisionByZero
  StackUnderflow
  InvalidWord
  UnknownWord
  EmptyProgram
}

pub fn new() -> Forth {
  Forth(stack: [], env: stdlib())
}

// Trivial pattern for a function. But if types change or we need to handle other things then having
// a separate function is nice.
fn stack_push(s: List(a), i) -> List(a) {
  [i, ..s]
}

fn stack_pop(s: List(a)) -> Result(#(a, List(a)), ForthError) {
  case s {
    [] -> Error(StackUnderflow)
    [a, ..t] -> Ok(#(a, t))
  }
}

pub fn format_stack(f: Forth) -> String {
  f.stack
  |> list.reverse
  |> list.map(int.to_string)
  |> list.intersperse(with: " ")
  |> string_builder.from_strings
  |> string_builder.to_string
}

fn tokenise_word(word: String, rest: List(String)) -> #(ForthTok, List(String)) {
  case word {
    ":" -> {
      let #([first_word, ..instructions], [_, ..rest]) =
        list.split_while(rest, fn(c) { c != ";" })
      #(
        WordDef(string.uppercase(first_word), UserDef(tokenise(instructions))),
        rest,
      )
    }
    _ ->
      case int.parse(word) {
        Ok(i) -> #(Value(i), rest)
        Error(_) -> #(Word(string.uppercase(word)), rest)
      }
  }
}

fn tokenise(instructions: List(String)) -> List(ForthTok) {
  do_tokenise(instructions, [])
}

fn do_tokenise(
  instructions: List(String),
  acc: List(ForthTok),
) -> List(ForthTok) {
  case instructions {
    [] -> list.reverse(acc)
    [word, ..rest] -> {
      let #(token, rest) = tokenise_word(word, rest)
      do_tokenise(rest, [token, ..acc])
    }
  }
}

fn execute(forth: Forth, tokens: List(ForthTok)) -> Result(Forth, ForthError) {
  case stack_pop(tokens) {
    Ok(#(token, rest)) -> {
      use new_forth <- result.then(eval_token(forth, token))
      execute(new_forth, rest)
    }
    Error(_) -> Ok(forth)
  }
}

fn binary_op(
  forth: Forth,
  op: fn(Int, Int) -> Result(Int, ForthError),
) -> Result(Forth, ForthError) {
  use #(a, new_stack) <- result.then(stack_pop(forth.stack))
  use #(b, new_stack) <- result.then(stack_pop(new_stack))
  use i <- result.then(op(b, a))
  // order is important
  Ok(Forth(..forth, stack: stack_push(new_stack, i)))
}

fn execute_builtin(f: Forth, builtin: String) -> Result(Forth, ForthError) {
  case builtin {
    "+" -> binary_op(f, fn(a, b) { Ok(a + b) })
    "-" -> binary_op(f, fn(a, b) { Ok(a - b) })
    "*" -> binary_op(f, fn(a, b) { Ok(a * b) })
    "/" ->
      binary_op(
        f,
        fn(a, b) {
          case b {
            0 -> Error(DivisionByZero)
            _ -> Ok(a / b)
          }
        },
      )
    "DUP" -> {
      use #(h, rest0) <- result.then(stack_pop(f.stack))
      let stack =
        stack_push(rest0, h)
        |> stack_push(h)
      Ok(Forth(..f, stack: stack))
    }
    "DROP" -> {
      use #(_, rest0) <- result.then(stack_pop(f.stack))
      Ok(Forth(..f, stack: rest0))
    }
    "SWAP" -> {
      use #(a, rest0) <- result.then(stack_pop(f.stack))
      use #(b, rest1) <- result.then(stack_pop(rest0))
      let stack =
        stack_push(rest1, a)
        |> stack_push(b)
      Ok(Forth(..f, stack: stack))
    }
    "OVER" -> {
      use #(a, rest0) <- result.then(stack_pop(f.stack))
      use #(b, rest1) <- result.then(stack_pop(rest0))
      let stack =
        stack_push(rest1, b)
        |> stack_push(a)
        |> stack_push(b)
      Ok(Forth(..f, stack: stack))
    }
  }
}

fn eval_token(forth: Forth, token: ForthTok) -> Result(Forth, ForthError) {
  case token {
    Value(v) -> Ok(Forth(..forth, stack: stack_push(forth.stack, v)))
    Word(w) ->
      // load word and evaluate with stack
      case dict.get(forth.env, w) {
        // You're on your own when you redefine a built in. :)
        Ok(UserDef(body)) -> execute(forth, body)
        Ok(BuiltIn(bin)) -> execute_builtin(forth, bin)
        Error(_) -> Error(UnknownWord)
      }
    WordDef(w, def) ->
      // define new word
      case int.parse(w) {
        // room for improvement here, but this is sufficent for our needs.
        Ok(_) -> Error(InvalidWord)
        Error(_) -> {
          // All the words in the definition need to be looked up in the environment
          use updated_def <- result.then(resolve_def(def, forth.env))
          Ok(Forth(..forth, env: dict.insert(forth.env, w, updated_def)))
        }
      }
  }
}

fn resolve_def(
  definition_body: ForthWordDef,
  env: dict.Dict(String, ForthWordDef),
) -> Result(ForthWordDef, ForthError) {
  case definition_body {
    BuiltIn(_) -> Ok(definition_body)
    UserDef(tokens) ->
      tokens
      |> list.map(lookup_token(_, env))
      |> flatten_results
      |> result.map(list.flatten)
      |> result.map(UserDef)
  }
}

fn lookup_token(
  token: ForthTok,
  env: dict.Dict(String, ForthWordDef),
) -> Result(List(ForthTok), ForthError) {
  case token {
    Word(w) ->
      case dict.get(env, w) {
        Ok(UserDef(tokens)) -> Ok(tokens)
        Ok(BuiltIn(body)) -> Ok([Word(body)])
        Error(_) -> Error(UnknownWord)
      }
    _ -> Ok([token])
  }
}

fn flatten_results(list: List(Result(a, b))) -> Result(List(a), b) {
  do_flatten_results(list, [])
}

fn do_flatten_results(list: List(Result(a, b)), acc: List(a)) {
  case list {
    [] -> Ok(list.reverse(acc))
    [Ok(result), ..rest] -> do_flatten_results(rest, [result, ..acc])
    [Error(error), ..] -> Error(error)
  }
}

pub fn eval(f: Forth, prog: String) -> Result(Forth, ForthError) {
  string.split(prog, on: " ")
  |> tokenise
  |> execute(f, _)
}
