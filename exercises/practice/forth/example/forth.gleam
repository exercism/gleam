import gleam/result
import gleam/io
import gleam/int
import gleam/map
import gleam/order
import gleam/bool
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
  Forth(stack: List(Int), env: map.Map(String, ForthWordDef))
}

pub fn stdlib() -> map.Map(String, ForthWordDef) {
  map.new()
  |> map.insert("+", BuiltIn("+"))
  |> map.insert("-", BuiltIn("-"))
  |> map.insert("/", BuiltIn("/"))
  |> map.insert("*", BuiltIn("*"))
  |> map.insert("DUP", BuiltIn("DUP"))
  |> map.insert("DROP", BuiltIn("DROP"))
  |> map.insert("SWAP", BuiltIn("SWAP"))
  |> map.insert("OVER", BuiltIn("OVER"))
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

fn intercalate(x: a, xs: List(a)) -> List(a) {
  case xs {
    [] -> []
    [h] -> [h]
    [h, ..t] -> [h, x, ..intercalate(x, t)]
  }
}

// Trivial pattern for a function. But if types change or we need to handle other things then having
// a separate function is nice.
fn stack_push(s: List(a), i) -> List(a) {
  [i, ..s]
}

fn stack_pop(s: List(a)) -> Result(tuple(a, List(a)), ForthError) {
  case s {
    [] -> Error(StackUnderflow)
    [a, ..t] -> Ok(tuple(a, t))
  }
}

pub fn format_stack(f: Forth) -> String {
  f.stack
  |> list.reverse
  |> list.map(int.to_string)
  |> intercalate(" ", _)
  |> string_builder.from_strings
  |> string_builder.to_string
}

fn toke_word(w: String, t: List(String)) -> tuple(ForthTok, List(String)) {
  case w {
    ":" -> {
      let tuple([w, ..def], [_, ..rest]) =
        list.split_while(
          t,
          fn(c) {
            case string.compare(c, ";") {
              order.Eq -> False
              _ -> True
            }
          },
        )
      tuple(WordDef(string.uppercase(w), UserDef(tokenise(def, []))), rest)
    }

    v ->
      case int.parse(v) {
        Ok(i) -> tuple(Value(i), t)
        Error(_) -> tuple(Word(string.uppercase(v)), t)
      }
  }
}

fn tokenise(ins: List(String), toks: List(ForthTok)) -> List(ForthTok) {
  case ins {
    [] -> toks
    [w] -> {
      let tuple(t, _) = toke_word(w, [])
      [t, ..toks]
    }
    [h, ..t] -> {
      let tuple(tok, ts) = toke_word(h, t)
      tokenise(ts, [tok, ..toks])
    }
  }
}

fn execute(f: Forth, xs: List(ForthTok)) -> Result(Forth, ForthError) {
  case stack_pop(xs) {
    Ok(tuple(i, rest0)) -> {
      try f0 = eval_token(f, i)
      execute(f0, rest0)
    }
    Error(_) -> Ok(f)
  }
}

fn binary_op(
  f: Forth,
  op: fn(Int, Int) -> Result(Int, ForthError),
) -> Result(Forth, ForthError) {
  try tuple(a, rest) = stack_pop(f.stack)
  try tuple(b, rest0) = stack_pop(rest)
  try i = op(b, a)
  // order is important
  Ok(Forth(..f, stack: stack_push(rest0, i)))
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
      try tuple(h, rest0) = stack_pop(f.stack)
      Ok(
        Forth(
          ..f,
          stack: stack_push(rest0, h)
          |> stack_push(h),
        ),
      )
    }
    "DROP" -> {
      try tuple(_, rest0) = stack_pop(f.stack)
      Ok(Forth(..f, stack: rest0))
    }
    "SWAP" -> {
      try tuple(a, rest0) = stack_pop(f.stack)
      try tuple(b, rest1) = stack_pop(rest0)
      Ok(
        Forth(
          ..f,
          stack: stack_push(rest1, a)
          |> stack_push(b),
        ),
      )
    }
    "OVER" -> {
      try tuple(a, rest0) = stack_pop(f.stack)
      try tuple(b, rest1) = stack_pop(rest0)
      Ok(
        Forth(
          ..f,
          stack: stack_push(rest1, b)
          |> stack_push(a)
          |> stack_push(b),
        ),
      )
    }
  }
}

fn eval_token(f: Forth, tok: ForthTok) -> Result(Forth, ForthError) {
  case tok {
    Value(v) -> Ok(Forth(..f, stack: stack_push(f.stack, v)))
    Word(w) ->
      // load word and evaluate with stack
      case map.get(f.env, w) {
        // You're on your own when you redefine a built in. :)
        Ok(UserDef(body)) -> execute(f, body)
        Ok(BuiltIn(bin)) -> execute_builtin(f, bin)
        Error(_) -> Error(UnknownWord)
      }
    WordDef(w, def) ->
      // define new word
      case int.parse(w) {
        // room for improvement here, but this is sufficent for our needs.
        Ok(_) -> Error(InvalidWord)
        Error(_) -> Ok(Forth(..f, env: map.insert(f.env, w, def)))
      }
  }
}

pub fn eval(f: Forth, prog: String) -> Result(Forth, ForthError) {
  string.split(prog, on: " ")
  |> tokenise([])
  |> list.reverse
  |> execute(f, _)
}
