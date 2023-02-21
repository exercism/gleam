import gleam/string
import gleam/int

pub type Clock {
  Clock(minutes: Int)
}

pub fn create(hour hour: Int, minute minute: Int) -> Clock {
  assert Ok(minutes) =
    int.modulo(hour * minutes_per_hour + minute, minutes_per_day)

  Clock(minutes)
}

pub fn add(clock: Clock, minutes minutes: Int) -> Clock {
  create(hour: 0, minute: clock.minutes + minutes)
}

pub fn subtract(clock: Clock, minutes minutes: Int) -> Clock {
  create(hour: 0, minute: clock.minutes - minutes)
}

pub fn display(clock: Clock) -> String {
  let hours =
    clock.minutes / minutes_per_hour
    |> int.to_string()
    |> string.pad_left(to: 2, with: "0")

  let minutes =
    clock.minutes % minutes_per_hour
    |> int.to_string()
    |> string.pad_left(to: 2, with: "0")

  hours <> ":" <> minutes
}

const minutes_per_hour = 60

const minutes_per_day = 1440
