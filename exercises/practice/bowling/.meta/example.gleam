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
  frame.rolls
  |> score_rolls <= 10
}

fn is_frame_open(frame: Frame) -> Bool {
  frame.rolls
  |> list.length == 2 && frame.rolls
  |> score_rolls < 10
}

fn is_frame_strike(frame: Frame) -> Bool {
  frame.rolls
  |> list.length == 1 && frame.rolls
  |> score_rolls == 10
}

fn is_frame_spare(frame: Frame) -> Bool {
  frame.rolls
  |> list.length == 2 && frame.rolls
  |> score_rolls == 10
}

fn is_frame_bonus_done(frame: Frame) -> Bool {
  frame
  |> is_frame_spare && frame.bonus
  |> list.length == 1 || frame
  |> is_frame_strike && frame.bonus
  |> list.length == 2
}

fn is_frame_complete(frame: Frame) -> Bool {
  frame
  |> is_frame_open || frame
  |> is_frame_bonus_done
}

fn is_frame_bonus_valid(frame: Frame) -> Bool {
  case
    frame
    |> is_frame_open || frame
    |> is_frame_bonus_done
    |> bool.negate
  {
    True -> True
    False ->
      case
        frame
        |> is_frame_spare
      {
        True ->
          frame.bonus
          |> score_rolls <= 10
        False ->
          case
            frame.bonus
            |> list.at(0)
          {
            Ok(10) ->
              frame.bonus
              |> score_rolls <= 20
            Ok(_) ->
              frame.bonus
              |> score_rolls <= 10
            _ -> False
          }
      }
  }
}

fn is_frame_rolls_done(frame: Frame) -> Bool {
  frame.rolls
  |> list.length == 2 || frame
  |> is_frame_strike
}

fn score_frame(frame: Frame) -> Int {
  {
    frame.rolls
    |> score_rolls
  } + {
    frame.bonus
    |> score_rolls
  }
}

fn is_frame_valid(frame: Frame) -> Bool {
  is_frame_rolls_valid(frame) && is_frame_bonus_valid(frame)
}

fn add_roll_to_frame(frame: Frame, roll: Int) -> Frame {
  case
    frame
    |> is_frame_complete
  {
    True -> frame

    False ->
      case
        frame
        |> is_frame_spare || frame
        |> is_frame_strike
      {
        True ->
          Frame(
            rolls: frame.rolls,
            bonus: frame.bonus
            |> list.reverse
            |> list.prepend(roll)
            |> list.reverse,
          )
        False ->
          Frame(
            rolls: frame.rolls
            |> list.reverse
            |> list.prepend(roll)
            |> list.reverse,
            bonus: frame.bonus,
          )
      }
  }
}

fn is_game_done(game: Game) -> Bool {
  game.frames
  |> list.length == 10 && game.frames
  |> list.all(fn(f) {
    f
    |> is_frame_complete
  })
}

pub fn roll(game: Game, knocked_pins: Int) -> Result(Game, Error) {
  let game = case
    game.frames
    |> list.length == 0
  {
    True -> Game([Frame([], [])])
    False -> game
  }

  case knocked_pins > 10 || knocked_pins < 0 {
    True -> Error(InvalidPinCount)
    False ->
      case
        game
        |> is_game_done
      {
        True -> Error(GameComplete)
        False -> {
          let new_game =
            Game(
              game.frames
              |> list.map(fn(f) {
                f
                |> add_roll_to_frame(knocked_pins)
              }),
            )
          case
            new_game.frames
            |> list.all(fn(f) {
              f
              |> is_frame_valid
            })
          {
            True ->
              case
                new_game.frames
                |> list.last
                |> result.unwrap(Frame([], []))
                |> is_frame_rolls_done && new_game.frames
                |> list.length < 10
              {
                True ->
                  Ok(Game(
                    frames: new_game.frames
                    |> list.reverse
                    |> list.prepend(Frame([], []))
                    |> list.reverse,
                  ))
                False -> Ok(new_game)
              }
            False -> Error(InvalidPinCount)
          }
        }
      }
  }
}

pub fn score(game: Game) -> Result(Int, Error) {
  case
    game
    |> is_game_done
  {
    False -> Error(GameNotComplete)
    True ->
      Ok(
        game.frames
        |> list.fold(
          0,
          fn(score, frame) {
            score + {
              frame
              |> score_frame
            }
          },
        ),
      )
  }
}
