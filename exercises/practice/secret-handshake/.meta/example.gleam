import gleam/bitwise
import gleam/list

pub type Command {
  Wink
  DoubleBlink
  CloseYourEyes
  Jump
}

pub fn commands(encoded_message: Int) -> List(Command) {
  [
    #(0b00000001, list.prepend(_, Wink)),
    #(0b00000010, list.prepend(_, DoubleBlink)),
    #(0b00000100, list.prepend(_, CloseYourEyes)),
    #(0b00001000, list.prepend(_, Jump)),
    #(0b00010000, list.reverse),
  ]
  |> list.fold(
    from: [],
    with: fn(acc, code_and_command) {
      let #(command_code, command) = code_and_command
      case bitwise.and(encoded_message, command_code) {
        0 -> acc
        _ -> command(acc)
      }
    },
  )
  |> list.reverse()
}
