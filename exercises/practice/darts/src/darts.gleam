import gleam/float

pub fn score(x x: Float, y y: Float) -> Int {
  let distance = float.square_root(x *. x +. y *. y)
  case distance {
    Ok(x) if x <. 2.0 -> 10
    Ok(x) if x <. 5.0 -> 5
    Ok(x) if x <. 14.0 -> 1
    _ -> 0
  }
}
