import gleam/list
import gleam/result
import gleam/string

pub fn clean(input: String) -> Result(String, String) {
  input
  |> remove_valid_punctuation
  |> validate_characters
  |> result.then(validate_length)
  |> result.then(validate_and_remove_country_code)
  |> result.then(validate_area_code)
  |> result.then(validate_exchange_code)
}

fn remove_valid_punctuation(input: String) -> String {
  input
  |> string.lowercase
  |> string.replace(" ", "")
  |> string.replace("(", "")
  |> string.replace(")", "")
  |> string.replace("+", "")
  |> string.replace(".", "")
  |> string.replace("-", "")
}

fn validate_characters(input: String) -> Result(String, String) {
  let digits = "1234567890"
  let letters = "abcdefghijklmnopqrstuvwxyz"
  let characters = string.to_graphemes(input)

  let all_digits = list.all(characters, string.contains(digits, _))
  let any_letters = list.any(characters, string.contains(letters, _))

  case Nil {
    _ if all_digits -> Ok(input)
    _ if any_letters -> Error("letters not permitted")
    _ -> Error("punctuations not permitted")
  }
}

fn validate_length(input: String) -> Result(String, String) {
  case string.length(input) {
    n if n < 10 -> Error("must not be fewer than 10 digits")
    n if n > 11 -> Error("must not be greater than 11 digits")
    _ -> Ok(input)
  }
}

fn validate_and_remove_country_code(input: String) -> Result(String, String) {
  let length = string.length(input)
  case string.first(input) {
    Ok("1") if length == 11 -> Ok(string.drop_left(input, 1))
    _ if length == 11 -> Error("11 digits must start with 1")
    _ -> Ok(input)
  }
}

fn validate_area_code(input: String) -> Result(String, String) {
  case string.first(input) {
    Ok("1") -> Error("area code cannot start with one")
    Ok("0") -> Error("area code cannot start with zero")
    _ -> Ok(input)
  }
}

fn validate_exchange_code(input: String) -> Result(String, String) {
  let from_exchange_code = string.drop_left(input, 3)
  case string.first(from_exchange_code) {
    Ok("0") -> Error("exchange code cannot start with zero")
    Ok("1") -> Error("exchange code cannot start with one")
    _ -> Ok(input)
  }
}
