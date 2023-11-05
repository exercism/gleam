import gleam/io
import gleam/map.{type Map}
import gleam/function
import gleam/list
import gleam/int
import gleam/float
import gleam/string
import gleam/result
import gleam/erlang
import gleam/erlang/file.{type Reason}
import gleam/json
import gleam/dynamic.{type DecodeError, type Dynamic}

// Types

type CanonicalData {
  CanonicalData(comments: List(String), cases: List(TestCase))
}

type TestCase {
  SingleTest(TestData)
  TestGroup(comments: List(String), description: String, cases: List(TestCase))
}

type TestData {
  TestData(
    comments: List(String),
    reimplements: Result(String, Nil),
    description: String,
    function: String,
    input: Map(String, JsonData),
    expected: JsonData,
  )
}

type Argument {
  Argument(arg_name: String, arg_type: JsonData)
}

type Function {
  Function(
    arguments: List(Argument),
    return_type: JsonData,
    can_error: Bool,
    need_labels: Bool,
    order: Int,
  )
}

type JsonData {
  JsonNull
  JsonBool(Bool)
  JsonInt(Int)
  JsonFloat(Float)
  JsonString(String)
  JsonList(List(JsonData))
  JsonObject(Map(String, JsonData))
}

fn json_data_to_gleam_type(data: JsonData) -> String {
  case data {
    JsonNull -> "Result(unknown, Nil)"
    JsonBool(_) -> "Bool"
    JsonInt(_) -> "Int"
    JsonFloat(_) -> "Float"
    JsonString(_) -> "String"
    JsonList([]) -> "List(unknown)"
    JsonList([head, ..]) -> "List(" <> json_data_to_gleam_type(head) <> ")"
    JsonObject(_) -> "CustomRecordType"
  }
}

fn json_data_to_gleam_value(data: JsonData) -> String {
  case data {
    JsonNull -> "Nil"
    JsonBool(True) -> "True"
    JsonBool(False) -> "False"
    JsonInt(int) -> int.to_string(int)
    JsonFloat(float) -> float.to_string(float)
    JsonString(string) -> string.inspect(string)
    JsonList(list) ->
      "[" <> string.join(list.map(list, json_data_to_gleam_value), ", ") <> "]"
    JsonObject(map) -> {
      let args =
        map
        |> map.to_list
        |> list.map(fn(arg) {
          clean_variable(arg.0) <> ": " <> json_data_to_gleam_value(arg.1)
        })
        |> string.join(", ")

      "CustomRecordType(" <> args <> ")"
    }
  }
}

fn have_same_type(a: JsonData, b: JsonData) -> Bool {
  case a, b {
    JsonNull, JsonNull -> True
    JsonBool(_), JsonBool(_) -> True
    JsonInt(_), JsonInt(_) -> True
    JsonFloat(_), JsonFloat(_) -> True
    JsonString(_), JsonString(_) -> True
    JsonList([x, ..]), JsonList([y, ..]) -> have_same_type(x, y)
    JsonObject(x), JsonObject(y) -> {
      let x: List(#(String, JsonData)) =
        x
        |> map.to_list
        |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
      let y: List(#(String, JsonData)) =
        y
        |> map.to_list
        |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
      let comparaison =
        list.strict_zip(x, y)
        |> result.map(list.all(_, fn(pair) {
          let #(#(name_a, type_a), #(name_b, type_b)) = pair
          name_a == name_b && have_same_type(type_a, type_b)
        }))
      comparaison == Ok(True)
    }
    _, _ -> False
  }
}

// Main

pub fn main() {
  let assert [slug, canonical_data] = erlang.start_arguments()

  case json.decode(from: canonical_data, using: canonical_data_decoder()) {
    Ok(data) -> {
      let _ = write_solution_files(slug, data)
      let _ = write_test_file(slug, data)
      io.println("Files written")
    }

    Error(errors) ->
      io.println("Error decoding canonical data:\n" <> string.inspect(errors))
  }
}

// Helpers

fn kebab_to_snake(slug: String) -> String {
  string.replace(slug, "-", "_")
}

fn camel_to_snake(variable: String) -> String {
  variable
  |> string.to_graphemes
  |> list.map(fn(char) {
    case string.lowercase(char) != char {
      True -> "_" <> string.lowercase(char)
      False -> char
    }
  })
  |> string.concat
}

fn clean_variable(variable: String) -> String {
  let reserved_words = [
    "as", "assert", "case", "const", "external", "fn", "if", "import", "let",
    "opaque", "pub", "todo", "try", "type", "use",
  ]
  let variable = camel_to_snake(variable)

  case list.contains(reserved_words, variable) {
    True -> variable <> "_value"
    False -> variable
  }
}

// Writing files

fn write_solution_files(
  slug: String,
  data: CanonicalData,
) -> Result(Nil, Reason) {
  let functions = functions_to_implement(data.cases)

  let content =
    functions
    |> map.to_list
    |> list.sort(by: fn(a, b) { int.compare({ a.1 }.order, { b.1 }.order) })
    |> list.map(fn(item) {
      let #(name, Function(arguments, return_type, can_error, need_labels, _)) =
        item
      let return_type = json_data_to_gleam_type(return_type)

      let return = case can_error {
        True -> "Result(" <> return_type <> ", String)"
        False -> return_type
      }

      let args =
        arguments
        |> list.map(fn(argument) {
          case need_labels {
            True ->
              string.concat([
                clean_variable(argument.arg_name),
                " ",
                clean_variable(argument.arg_name),
                ": ",
                json_data_to_gleam_type(argument.arg_type),
              ])
            False ->
              string.concat([
                clean_variable(argument.arg_name),
                ": ",
                json_data_to_gleam_type(argument.arg_type),
              ])
          }
        })
        |> string.join(", ")

      "pub fn " <> clean_variable(name) <> "(" <> args <> ") -> " <> return <> " {\n todo \n}"
    })
    |> string.join("\n")

  let exercise = kebab_to_snake(slug)

  let solution_path =
    string.join(
      ["..", "exercises", "practice", slug, "src", exercise <> ".gleam"],
      "/",
    )
  let assert Ok(Nil) = file.write(content, solution_path)

  let example_path =
    string.join(
      ["..", "exercises", "practice", slug, ".meta", "example.gleam"],
      "/",
    )

  let assert Ok(Nil) = file.write(content, example_path)
}

fn functions_to_implement(test_cases: List(TestCase)) -> Map(String, Function) {
  list.fold(over: test_cases, from: map.new(), with: check_test_case)
}

fn check_test_case(
  functions: Map(String, Function),
  test_case: TestCase,
) -> Map(String, Function) {
  case test_case {
    TestGroup(cases: cases, ..) ->
      list.fold(over: cases, from: functions, with: check_test_case)
    SingleTest(TestData(
      function: function,
      expected: expected,
      input: input,
      ..,
    )) -> {
      let can_error = case expected {
        JsonObject(object) -> map.has_key(object, "error")
        _ -> False
      }

      let args =
        input
        |> map.to_list
        |> list.map(fn(arg) { Argument(arg.0, arg.1) })

      let need_labels = case args {
        [] -> False
        [_] -> False
        [Argument(_, type_a), Argument(_, type_b)] ->
          have_same_type(type_a, type_b)
        _ -> True
      }

      let current_function = case map.get(functions, function) {
        Ok(func) -> {
          let func =
            Function(..func, need_labels: func.need_labels || need_labels)

          case can_error {
            True -> Function(..func, can_error: True)
            False -> Function(..func, return_type: expected)
          }
        }

        Error(Nil) ->
          Function(args, expected, can_error, need_labels, map.size(functions))
      }

      map.insert(functions, function, current_function)
    }
  }
}

fn write_test_file(slug: String, data: CanonicalData) {
  let exercise = kebab_to_snake(slug)
  let functions = functions_to_implement(data.cases)
  let comments = print_comments(data.comments)
  let test_cases = print_tests(slug, "", functions, data.cases)

  let content =
    "
  import exercism/test_runner
  import exercism/should
  import <exercise>

  <comments>

  pub fn main() {
    test_runner.main()
  }

  <test_cases>
  "
    |> string.replace("<exercise>", exercise)
    |> string.replace("<comments>", comments)
    |> string.replace("<test_cases>", test_cases)

  let path =
    string.join(
      ["..", "exercises", "practice", slug, "test", exercise <> "_test.gleam"],
      "/",
    )

  let assert Ok(Nil) = file.write(content, path)
}

fn print_comments(comments: List(String)) -> String {
  comments
  |> list.map(fn(comment) { "// " <> comment })
  |> string.join("\n")
}

fn print_tests(
  slug: String,
  prefix: String,
  functions: Map(String, Function),
  tests: List(TestCase),
) -> String {
  tests
  |> list.map(print_test(slug, prefix, functions, _))
  |> string.join("\n")
}

fn print_test(
  slug: String,
  prefix: String,
  functions: Map(String, Function),
  test: TestCase,
) -> String {
  case test {
    TestGroup(comments, description, cases) -> {
      let prefix = prefix <> "" <> description <> "_"
      let tests = print_tests(slug, prefix, functions, cases)
      print_comments(comments) <> "\n" <> tests
    }
    SingleTest(TestData(
      comments,
      reimplements,
      description,
      function,
      input,
      expected,
    )) -> {
      let exercise = kebab_to_snake(slug)
      let comments = case reimplements {
        Ok(uuid) ->
          [
            "This test reimplements the test with uuid " <> uuid,
            "Please identify that test and remove it. Link:",
            "https://github.com/exercism/problem-specifications/blob/main/exercises/" <> slug <> "/canonical-data.json",
          ]
          |> list.append(comments)
          |> print_comments()
        _ -> print_comments(comments)
      }
      let test_name = flatten_description(prefix <> description)
      let assert Ok(Function(need_labels: need_labels, ..)) =
        map.get(functions, function)
      let input =
        input
        |> map.to_list
        |> list.map(fn(item) {
          case need_labels {
            True ->
              clean_variable(item.0) <> ": " <> json_data_to_gleam_value(item.1)
            False -> json_data_to_gleam_value(item.1)
          }
        })
        |> string.join(", ")
      let expected = get_expected_value(function, functions, expected)

      "
      <comments>
      pub fn <test_name>_test(){
        <exercise>.<function>(<input>)
        |> should.equal(<expected>)
      }
      "
      |> string.replace("<comments>", comments)
      |> string.replace("<test_name>", test_name)
      |> string.replace("<exercise>", exercise)
      |> string.replace("<function>", clean_variable(function))
      |> string.replace("<input>", input)
      |> string.replace("<expected>", expected)
    }
  }
}

fn flatten_description(description: String) -> String {
  description
  |> string.lowercase
  |> string.to_graphemes
  |> list.map(fn(char) {
    let allowed_chars = "0123456789abcdefghijklmnopqrstuvwxyz_"
    case string.contains(allowed_chars, char) {
      True -> char
      False -> "_"
    }
  })
  |> string.concat
  |> string.replace(each: "__", with: "_")
}

fn get_expected_value(
  function: String,
  functions: Map(String, Function),
  expected: JsonData,
) {
  case map.get(functions, function) {
    Ok(Function(can_error: True, ..)) ->
      case expected {
        JsonObject(map) ->
          case map.get(map, "error") {
            Ok(value) -> "Error(" <> json_data_to_gleam_value(value) <> ")"
            Error(Nil) -> "Ok(" <> json_data_to_gleam_value(expected) <> ")"
          }
        _ -> "Ok(" <> json_data_to_gleam_value(expected) <> ")"
      }

    _ -> json_data_to_gleam_value(expected)
  }
}

// Decoders

type Decoder(a) =
  fn(Dynamic) -> Result(a, List(DecodeError))

fn canonical_data_decoder() -> Decoder(CanonicalData) {
  dynamic.decode2(
    CanonicalData,
    comments_decoder(),
    dynamic.field("cases", dynamic.list(case_decoder())),
  )
}

fn comments_decoder() -> Decoder(List(String)) {
  dynamic.any([
    dynamic.field("comments", dynamic.list(dynamic.string)),
    function.constant(Ok([])),
  ])
}

fn case_decoder() -> Decoder(TestCase) {
  dynamic.any([
    dynamic.decode1(SingleTest, test_decoder()),
    dynamic.decode3(
      TestGroup,
      comments_decoder(),
      dynamic.field("description", dynamic.string),
      dynamic.field("cases", dynamic.list(lazy(fn() { case_decoder() }))),
    ),
  ])
}

fn test_decoder() -> Decoder(TestData) {
  dynamic.decode6(
    TestData,
    comments_decoder(),
    optional(dynamic.field("reimplements", dynamic.string)),
    dynamic.field("description", dynamic.string),
    dynamic.field("property", dynamic.string),
    dynamic.field("input", dynamic.map(dynamic.string, json_data_decoder())),
    dynamic.field("expected", json_data_decoder()),
  )
}

fn json_data_decoder() -> Decoder(JsonData) {
  dynamic.any([
    dynamic.decode1(JsonBool, dynamic.bool),
    dynamic.decode1(JsonInt, dynamic.int),
    dynamic.decode1(JsonFloat, dynamic.float),
    dynamic.decode1(JsonString, dynamic.string),
    dynamic.decode1(JsonList, dynamic.list(lazy(fn() { json_data_decoder() }))),
    dynamic.decode1(
      JsonObject,
      dynamic.map(dynamic.string, lazy(fn() { json_data_decoder() })),
    ),
    function.constant(Ok(JsonNull)),
  ])
}

fn optional(decoder: Decoder(a)) -> Decoder(Result(a, Nil)) {
  dynamic.any([dynamic.decode1(Ok, decoder), function.constant(Ok(Error(Nil)))])
}

fn lazy(wrapped_decoder: fn() -> Decoder(a)) -> Decoder(a) {
  fn(data) {
    let decoder = wrapped_decoder()
    decoder(data)
  }
}
