import gleam/list

pub fn recite(inputs: List(String)) -> List(String) {
  case inputs {
    [] -> []
    [subject, ..] ->
      inputs
      |> list.window(2)
      |> list.map(fn(pair) {
        case pair {
          [want, lost] ->
            "For want of a " <> want <> " the " <> lost <> " was lost."
        }
      })
      |> list.append(["And all for the want of a " <> subject <> "."])
  }
}
