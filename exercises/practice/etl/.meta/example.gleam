import gleam/string
import gleam/map.{Map}
import gleam/list

pub fn transform(legacy: Map(Int, List(String))) -> Map(String, Int) {
  map.fold(
    over: legacy,
    from: map.new(),
    with: fn(transformed, score, letters) {
      list.fold(
        over: letters,
        from: transformed,
        with: fn(transformed, letter) {
          map.insert(transformed, string.lowercase(letter), score)
        },
      )
    },
  )
}
