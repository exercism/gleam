import gleam/string
import gleam/list

pub type Robot {
  Robot(direction: Direction, position: Position)
}

pub type Direction {
  North
  East
  South
  West
}

pub type Position {
  Position(x: Int, y: Int)
}

pub fn create(
  direction direction: Direction,
  position position: Position,
) -> Robot {
  Robot(direction, position)
}

pub fn move(
  direction direction: Direction,
  instructions instructions: String,
  position position: Position,
) -> Robot {
  let robot = create(direction, position)
  instructions
  |> string.to_graphemes
  |> list.fold(from: robot, with: move_one)
}

fn move_one(robot: Robot, instruction: String) -> Robot {
  let Robot(direction, position) = robot
  case instruction {
    "L" -> Robot(turn_left(direction), position)
    "R" -> Robot(turn_right(direction), position)
    "A" -> Robot(direction, advance(direction, position))
    _ -> robot
  }
}

fn turn_left(direction: Direction) -> Direction {
  case direction {
    North -> West
    East -> North
    South -> East
    West -> South
  }
}

fn turn_right(direction: Direction) -> Direction {
  case direction {
    North -> East
    East -> South
    South -> West
    West -> North
  }
}

fn advance(direction: Direction, position: Position) -> Position {
  let Position(x, y) = position
  case direction {
    North -> Position(x, y + 1)
    East -> Position(x + 1, y)
    South -> Position(x, y - 1)
    West -> Position(x - 1, y)
  }
}
