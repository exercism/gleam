import gleam/int
import gleam/list
import gleam/float

pub fn lowest_price(books: List(Int)) -> Float {
  books
  |> list.fold(
    [#(1, 0), #(2, 0), #(3, 0), #(4, 0), #(5, 0)],
    fn(acc, book) {
      let assert Ok(current) = list.key_find(acc, book)
      list.key_set(acc, book, current + 1)
    },
  )
  |> list.map(fn(item) { item.1 })
  |> list.sort(int.compare)
  |> calculate(0.0)
}

fn calculate(counts: List(Int), acc: Float) -> Float {
  case counts {
    [0, 0, 0, 0, a] -> acc +. int.to_float(a) *. 800.0

    [0, 0, 0, a, b] -> calculate([0, 0, 0, a - 1, b - 1], acc +. 2.0 *. 760.0)

    [0, 0, a, b, c] ->
      calculate([0, 0, a - 1, b - 1, c - 1], acc +. 3.0 *. 720.0)

    [0, a, b, c, d] ->
      calculate([0, a - 1, b - 1, c - 1, d - 1], acc +. 4.0 *. 640.0)

    [a, b, c, d, e] ->
      float.min(
        calculate([a - 1, b - 1, c - 1, d - 1, e - 1], acc +. 5.0 *. 600.0),
        calculate(
          list.sort([a, b - 1, c - 1, d - 1, e - 1], int.compare),
          acc +. 4.0 *. 640.0,
        ),
      )
  }
}
