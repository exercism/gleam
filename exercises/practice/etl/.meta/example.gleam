import gleam/string
import gleam/dict.{type Dict}
import gleam/list

pub fn transform(legacy: Dict(Int, List(String))) -> Dict(String, Int) {
  dict.fold(
    over: legacy,
    from: dict.new(),
    with: fn(transformed, score, letters) {
      list.fold(
        over: letters,
        from: transformed,
        with: fn(transformed, letter) {
          dict.insert(transformed, string.lowercase(letter), score)
        },
      )
    },
  )
}
