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
  todo
}

pub fn move(
  direction direction: Direction,
  instructions instructions: String,
  position position: Position,
) -> Robot {
  todo
}
