import gleam/list
import gleam/bool
import gleam/result

pub opaque type Frame {
  Frame(rolls: List(Int), bonus: List(Int))
}

pub type Game {
  Game(frames: List(Frame))
}

pub type Error {
  InvalidPinCount
  GameComplete
  GameNotComplete
}

fn score_rolls(rolls: List(Int)) -> Int {
  rolls
  |> list.fold(0, fn(sum, pins) { sum + pins })
}

fn is_frame_rolls_valid(frame: Frame) -> Bool {
  score_rolls(frame.rolls) <= 10
}

fn is_frame_open(frame: Frame) -> Bool {
  list.length(frame.rolls) == 2 && frame.rolls(score_rolls) < 10
}

fn is_frame_strike(frame: Frame) -> Bool {
  list.length(frame.rolls) == 1 && score_rolls(frame.rolls) == 10
}

fn is_frame_spare(frame: Frame) -> Bool {
  list.length(frame.rolls) == 2 && score_rolls(frame.rolls) == 10
}

fn is_frame_bonus_done(frame: Frame) -> Bool {
  is_frame_spare(frame) && list.length(frame.bonus) == 1 || is_frame_strike(
    frame,
  ) && list.length(frame.bonus) == 2
}

fn is_frame_complete(frame: Frame) -> Bool {
  is_frame_open(frame) || is_frame_bonus_done(frame)
}

fn is_frame_bonus_valid(frame: Frame) -> Bool {
  case is_frame_open(frame) || !is_frame_bonus_done(frame) {
    True -> True
    False ->
      case is_frame_spare(frame) {
        True -> score_rolls(frame.bonus) <= 10
        False ->
          case frame.bonus {
            [10, ..] -> score_rolls(frame.bonus) <= 20
            [_, ..] -> score_rolls(frame.bonus) <= 10
            _ -> False
          }
      }
  }
}

fn is_frame_rolls_done(frame: Frame) -> Bool {
  list.length(frame.rolls) == 2 || is_frame_strike(frame)
}

fn score_frame(frame: Frame) -> Int {
  score_rolls(frame.rolls) + score_rolls(frame.bonus)
}

fn is_frame_valid(frame: Frame) -> Bool {
  is_frame_rolls_valid(frame) && is_frame_bonus_valid(frame)
}

fn add_roll_to_frame(frame: Frame, roll: Int) -> Frame {
  case is_frame_complete(frame) {
    True -> frame

    False ->
      case is_frame_spare(frame) || is_frame_strike(frame) {
        True -> {
          let bonus =
            frame.bonus
            |> list.reverse
            |> list.prepend(roll)
            |> list.reverse
          Frame(rolls: frame.rolls, bonus: bonus)
        }
        False -> {
          let rolls =
            frame.rolls
            |> list.reverse
            |> list.prepend(roll)
            |> list.reverse
          Frame(rolls: rolls, bonus: frame.bonus)
        }
      }
  }
}

fn is_game_done(game: Game) -> Bool {
  list.length(game.frames) == 10 && list.all(game.frames, is_frame_complete)
}

pub fn roll(game: Game, knocked_pins: Int) -> Result(Game, Error) {
  let game = case game.frames {
    [] -> Game([Frame([], [])])
    _ -> game
  }

  case knocked_pins > 10 || knocked_pins < 0 {
    True -> Error(InvalidPinCount)
    False ->
      case is_game_done(game) {
        True -> Error(GameComplete)
        False -> {
          let new_game =
            Game(
              game.frames
              |> list.map(add_roll_to_frame(_, knocked_pins)),
            )
          case list.all(new_game.frames, is_frame_valid) {
            True -> {
              let last =
                list.last(new_game.frames)
                |> result.unwrap(Frame([], []))
              case
                is_frame_rolls_done(last) && list.length(new_game.frames) < 10
              {
                True -> {
                  let frames =
                    new_game.frames
                    |> list.reverse
                    |> list.prepend(Frame([], []))
                    |> list.reverse
                  Ok(Game(frames: frames))
                }
                False -> Ok(new_game)
              }
            }
            False -> Error(InvalidPinCount)
          }
        }
      }
  }
}

pub fn score(game: Game) -> Result(Int, Error) {
  case is_game_done(game) {
    False -> Error(GameNotComplete)
    True ->
      game.frames
      |> list.fold(0, fn(score, frame) { score + score_frame(frame) })
      |> Ok
  }
}
