import gleam/string

pub type Error {
  OutOfRange
}

pub fn say(number: Int) -> Result(String, Error) {
  case number {
    0 -> Ok("zero")
    _ if number < 0 -> Error(OutOfRange)
    _ if number >= 1_000_000_000_000 -> Error(OutOfRange)
    _ ->
      number
      |> do_say
      |> string.replace("  ", " ")
      |> string.trim()
      |> Ok
  }
}

const billion = 1_000_000_000

const million = 1_000_000

const thousand = 1000

const hundred = 100

fn do_say(number: Int) -> String {
  case number {
    _ if number >= billion -> {
      let billions = number / billion
      let rest = number % billion
      do_say(billions) <> " billion " <> do_say(rest)
    }

    _ if number >= million -> {
      let millions = number / million
      let rest = number % million
      do_say(millions) <> " million " <> do_say(rest)
    }

    _ if number >= thousand -> {
      let thousands = number / thousand
      let rest = number % thousand
      do_say(thousands) <> " thousand " <> do_say(rest)
    }

    _ if number >= hundred -> {
      let hundreds = number / hundred
      let rest = number % hundred
      do_say(hundreds) <> " hundred " <> do_say(rest)
    }

    _ if number >= 20 -> {
      let num_tens = number / 10
      let rest = number % 10
      case rest {
        0 -> tens(num_tens)
        _ -> tens(num_tens) <> "-" <> ones(rest)
      }
    }

    _ -> ones(number)
  }
}

fn ones(number: Int) -> String {
  case number {
    0 -> ""
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    10 -> "ten"
    11 -> "eleven"
    12 -> "twelve"
    13 -> "thirteen"
    14 -> "fourteen"
    15 -> "fifteen"
    16 -> "sixteen"
    17 -> "seventeen"
    18 -> "eighteen"
    _19 -> "nineteen"
  }
}

fn tens(number: Int) -> String {
  case number {
    2 -> "twenty"
    3 -> "thirty"
    4 -> "forty"
    5 -> "fifty"
    6 -> "sixty"
    7 -> "seventy"
    8 -> "eighty"
    _9 -> "ninety"
  }
}
