import gleam/list
import gleam/string

pub fn recite(inputs: List(String)) -> String {
  case inputs {
    [] -> ""
    [subject, ..] ->
      inputs
      |> list.window_by_2()
      |> list.map(fn(pair) {
        case pair {
          #(want, lost) ->
            "For want of a " <> want <> " the " <> lost <> " was lost."
        }
      })
      |> list.append(["And all for the want of a " <> subject <> "."])
      |> list.intersperse("\n")
      |> string.concat()
  }
}
