import gleam/string
import gleam/dynamic.{Dynamic}
import gleam/erlang.{Reference}
import gleam/erlang/atom.{Atom}

/// A `Pid` (or Process identifier) is a reference to an Erlang process. Each
/// process has a `Pid` and it is one of the lowest level building blocks of
/// inter-process communication in the Erlang and Gleam OTP frameworks.
///
pub external type Pid

/// Get the `Pid` for the current process.
pub external fn self() -> Pid =
  "erlang" "self"

/// Create a new Erlang process that runs concurrently to the creator. In other
/// languages this might be called a fibre, a green thread, or a coroutine.
///
/// If `linked` is `True` then the created process is linked to the creator
/// process. When a process terminates an exit signal is sent to all other
/// processes that are linked to it, causing the process to either terminate or
/// have to handle the signal.
///
/// More can be read about processes and links in the [Erlang documentation][1].
///
/// [1]: https://www.erlang.org/doc/reference_manual/processes.html
///
pub fn start(running implementation: fn() -> anything, linked link: Bool) -> Pid {
  case link {
    True -> spawn_link(implementation)
    False -> spawn(implementation)
  }
}

external fn spawn(fn() -> anything) -> Pid =
  "erlang" "spawn"

external fn spawn_link(fn() -> anything) -> Pid =
  "erlang" "spawn_link"

/// A `Subject` is a value that processes can use to send and receive messages
/// to and from each other in a well typed way.
///
/// Each subject is "owned" by the process that created it. Any process can use
/// the `send` function to sent a message of the correct type to the process
/// that owns the subject, and the owner can use the `receive` function or the
/// `Selector` type to receive these messages.
///
/// The `Subject` type is similar to the "channel" types found in other
/// languages and the "topic" concept found in some pub-sub systems.
///
/// # Examples
///
/// ```gleam
/// let subject = new_subject()
///
/// // Send a message with the subject
/// send(subject, "Hello, Joe!")
///
/// // Receive the message
/// receive(subject, within: 10)
/// ```
///
pub opaque type Subject(message) {
  Subject(owner: Pid, tag: Reference)
}

/// Create a new `Subject` owned by the current process.
///
pub fn new_subject() -> Subject(message) {
  Subject(owner: self(), tag: erlang.make_reference())
}

/// Get the owner process for a `Subject`. This is the process that created the
/// `Subject` and will receive messages sent with it.
///
pub fn subject_owner(subject: Subject(message)) -> Pid {
  subject.owner
}

external type DoNotLeak

external fn raw_send(Pid, message) -> DoNotLeak =
  "erlang" "send"

/// Send a message to a process using a `Subject`. The message must be of the
/// type that the `Subject` accepts.
///
/// This function does not wait for the `Subject` owner process to call the
/// `receive` function, instead it returns once the message has been placed in
/// the process' mailbox.
///
/// # Ordering
///
/// If process P1 sends two messages to process P2 it is guarenteed that process
/// P1 will receive the messages in the order they were sent.
///
/// If you wish to receive the messages in a different order you can send them
/// on two different subjects and the receiver function can call the `receive`
/// function for each subject in the desired order, or you can write some Erlang
/// code to perform a selective receive.
///
/// # Examples
///
/// ```gleam
/// let subject = new_subject()
/// send(subject, "Hello, Joe!")
/// ```
///
pub fn send(subject: Subject(message), message: message) -> Nil {
  raw_send(subject.owner, #(subject.tag, message))
  Nil
}

/// Receive a message that has been sent to current process using the `Subject`.
///
/// If there is not an existing message for the `Subject` in the process'
/// mailbox or one does not arrive `within` the permitted timeout then the
/// `Error(Nil)` is returned.
///
/// Only the process that is owner of the `Subject` can receive a message using
/// it. If a process that does not own the `Subject` attempts to receive with it
/// then it will not receive a message.
///
/// To wait for messages from multiple `Subject`s at the same time see the
/// `Selector` type.
///
pub fn receive(
  from subject: Subject(message),
  within milliseconds: Int,
) -> Result(message, Nil) {
  new_selector()
  |> selecting(subject, fn(x) { x })
  |> select(within: milliseconds)
}

/// A type that enables a process to wait for messages from multiple `Subject`s
/// at the same time, returning whichever message arrives first.
///
/// Used with the `new_selector`, `selecting`, and `select` functions.
///
/// # Examples
///
/// ```gleam
/// > let int_subject = new_subject()
/// > let float_subject = new_subject()
/// > send(int_subject, 1)
/// >
/// > let selector =
/// >   new_selector()
/// >   |> selecting(int_subject, int.to_string)
/// >   |> selecting(float_subject, float.to_string)
/// >
/// > select(selector)
/// Ok("1")
/// ```
///
pub external type Selector(payload)

/// Create a new `Selector` which can be used to receive messages on multiple
/// `Subject`s at once.
///
pub external fn new_selector() -> Selector(payload) =
  "gleam_erlang_ffi" "new_selector"

/// Receive a message that has been sent to current process using any of the
/// `Subject`s that have been added to the `Selector` with the `selecting`
/// function.
///
/// If there is not an existing message for the `Selector` in the process'
/// mailbox or one does not arrive `within` the permitted timeout then the
/// `Error(Nil)` is returned.
///
/// Only the process that is owner of the `Subject`s can receive a message using
/// them. If a process that does not own the a `Subject` attempts to receive
/// with it then it will not receive a message.
///
/// To wait forever for the next message rather than for a limited amount of
/// time see the `select_forever` function.
///
pub external fn select(
  from: Selector(payload),
  within: Int,
) -> Result(payload, Nil) =
  "gleam_erlang_ffi" "select"

/// Similar to the `select` function but will wait forever for a message to
/// arrive rather than timing out after a specified amount of time.
///
pub external fn select_forever(from: Selector(payload)) -> payload =
  "gleam_erlang_ffi" "select"

/// Add a transformation function to a selector. When a message is received
/// using this selector the tranformation function is applied to the message.
///
/// This function can be used to change the type of messages received and may
/// be useful when combined with the `merge_selector` function.
///
pub external fn map_selector(Selector(a), fn(a) -> b) -> Selector(b) =
  "gleam_erlang_ffi" "map_selector"

/// Merge one selector into another, producing a selector that contains the
/// message handlers of both.
///
/// If a subject is handled by both selectors the handler function of the
/// second selector is used.
///
pub external fn merge_selector(Selector(a), Selector(a)) -> Selector(a) =
  "gleam_erlang_ffi" "merge_selector"

pub type ExitMessage {
  ExitMessage(pid: Pid, reason: ExitReason)
}

pub type ExitReason {
  Normal
  Killed
  Abnormal(reason: String)
}

/// Add a handler for trapped exit messages. In order for these messages to be
/// sent to the process when a linked process exits the process must call the
/// `trap_exit` beforehand.
///
pub fn selecting_trapped_exits(
  selector: Selector(a),
  handler: fn(ExitMessage) -> a,
) -> Selector(a) {
  let tag = atom.create_from_string("EXIT")
  let handler = fn(message: #(Atom, Pid, Dynamic)) -> a {
    let reason = message.2
    let normal = dynamic.from(Normal)
    let killed = dynamic.from(Killed)
    let reason = case dynamic.string(reason) {
      _ if reason == normal -> Normal
      _ if reason == killed -> Killed
      Ok(reason) -> Abnormal(reason)
      Error(_) -> Abnormal(string.inspect(reason))
    }
    handler(ExitMessage(message.1, reason))
  }
  insert_selector_handler(selector, #(tag, 3), handler)
}

// TODO: implement in Gleam
/// Discard all messages in the current process' mailbox.
///
/// Warning: This function may cause other processes to crash if they sent a
/// message to the current process and are waiting for a response, so use with
/// caution.
///
pub external fn flush_messages() -> Nil =
  "gleam_erlang_ffi" "flush_messages"

/// Add a new `Subject` to the `Selector` to that it's messages can be received.
///
/// The `mapping` function provided with the `Subject` can be used to convert
/// the type of messages received using this `Subject`. This is useful for when
/// you wish to add multiple `Subject`s to a `Seletor` when they have differing
/// message types. If you do not wish to transform the incoming messages in any
/// way then the `identity` function can be given.
///
pub fn selecting(
  selector: Selector(payload),
  for subject: Subject(message),
  mapping transform: fn(message) -> payload,
) -> Selector(payload) {
  let handler = fn(message: #(Reference, message)) { transform(message.1) }
  insert_selector_handler(selector, #(subject.tag, 2), handler)
}

/// Add a handler to a selector for 2 element tuple messages with a given tag
/// element in the first position.
///
/// Typically you want to use the `selecting` function with a `Subject` instead,
/// but this function may be useful if you need to receive messages sent from
/// other BEAM languages that do not use the `Subject` type.
///
pub fn selecting_record2(
  selector: Selector(payload),
  tag: tag,
  mapping transform: fn(Dynamic) -> payload,
) -> Selector(payload) {
  let handler = fn(message: #(tag, Dynamic)) { transform(message.1) }
  insert_selector_handler(selector, #(tag, 2), handler)
}

/// Add a handler to a selector for 3 element tuple messages with a given tag
/// element in the first position.
///
/// Typically you want to use the `selecting` function with a `Subject` instead,
/// but this function may be useful if you need to receive messages sent from
/// other BEAM languages that do not use the `Subject` type.
///
pub fn selecting_record3(
  selector: Selector(payload),
  tag: tag,
  mapping transform: fn(Dynamic, Dynamic) -> payload,
) -> Selector(payload) {
  let handler = fn(message: #(tag, Dynamic, Dynamic)) {
    transform(message.1, message.2)
  }
  insert_selector_handler(selector, #(tag, 3), handler)
}

/// Add a handler to a selector for 4 element tuple messages with a given tag
/// element in the first position.
///
/// Typically you want to use the `selecting` function with a `Subject` instead,
/// but this function may be useful if you need to receive messages sent from
/// other BEAM languages that do not use the `Subject` type.
///
pub fn selecting_record4(
  selector: Selector(payload),
  tag: tag,
  mapping transform: fn(Dynamic, Dynamic, Dynamic) -> payload,
) -> Selector(payload) {
  let handler = fn(message: #(tag, Dynamic, Dynamic, Dynamic)) {
    transform(message.1, message.2, message.3)
  }
  insert_selector_handler(selector, #(tag, 4), handler)
}

type AnythingSelectorTag {
  Anything
}

/// Add a catch-all handler to a selector that will be used when no other
/// handler in a selector is suitable for a given message.
///
/// This may be useful for when you want to ensure that any message in the inbox
/// is handled, or when you need to handle messages from other BEAM languages
/// which do not use subjects or record format messages.
///
pub fn selecting_anything(
  selector: Selector(payload),
  mapping handler: fn(Dynamic) -> payload,
) -> Selector(payload) {
  insert_selector_handler(selector, Anything, handler)
}

external fn insert_selector_handler(
  Selector(payload),
  for: tag,
  mapping: fn(message) -> payload,
) -> Selector(payload) =
  "gleam_erlang_ffi" "insert_selector_handler"

/// Suspends the process calling this function for the specified number of
/// milliseconds.
///
pub external fn sleep(Int) -> Nil =
  "gleam_erlang_ffi" "sleep"

/// Suspends the process forever! This may be useful for suspending the main
/// process in a Gleam program when it has no more work to do but we want other
/// processes to continue to work.
///
pub external fn sleep_forever() -> Nil =
  "gleam_erlang_ffi" "sleep_forever"

/// Check to see whether the process for a given `Pid` is alive.
///
/// See the [Erlang documentation][1] for more information.
///
/// [1]: http://erlang.org/doc/man/erlang.html#is_process_alive-1
///
pub external fn is_alive(Pid) -> Bool =
  "erlang" "is_process_alive"

type ProcessMonitorFlag {
  Process
}

external fn erlang_monitor_process(ProcessMonitorFlag, Pid) -> Reference =
  "erlang" "monitor"

pub opaque type ProcessMonitor {
  ProcessMonitor(tag: Reference)
}

/// A message received when a monitored process exits.
///
pub type ProcessDown {
  ProcessDown(pid: Pid, reason: Dynamic)
}

/// Start monitoring a process so that when the monitored process exits a
/// message is to the monitoring process.
///
/// The message is only sent once, when the target process exits. If the
/// process was not alive when this function is called the message will never
/// be received.
///
/// The down message can be received with a `Selector` and the
/// `selecting_process_down` function.
///
/// The process can be demonitored with the `demonitor_process` function.
///
pub fn monitor_process(pid: Pid) -> ProcessMonitor {
  Process
  |> erlang_monitor_process(pid)
  |> ProcessMonitor
}

/// Add a `ProcessMonitor` to a `Selector` so that the `ProcessDown` message can
/// be received using the `Selector` and the `select` function.
///
pub fn selecting_process_down(
  selector: Selector(payload),
  monitor: ProcessMonitor,
  mapping: fn(ProcessDown) -> payload,
) -> Selector(payload) {
  insert_selector_handler(selector, monitor.tag, mapping)
}

/// Remove the monitor for a process so that when the monitor process exits a
/// `ProcessDown` message is not sent to the monitoring process.
///
/// If the message has already been sent it is removed from the monitoring
/// process' mailbox.
///
pub external fn demonitor_process(monitor: ProcessMonitor) -> Nil =
  "gleam_erlang_ffi" "demonitor"

/// An error returned when making a call to a process.
///
pub type CallError(msg) {
  /// The process being called exited before it sent a response.
  ///
  CalleeDown(reason: Dynamic)

  /// The process being called did not response within the permitted amount of
  /// time.
  ///
  CallTimeout
}

// This function is based off of Erlang's gen:do_call/4.
/// Send a message to a process and wait for a reply.
///
/// If the receiving process exits or does not reply within the allowed amount
/// of time then an error is returned.
///
pub fn try_call(
  subject: Subject(request),
  make_request: fn(Subject(response)) -> request,
  within timeout: Int,
) -> Result(response, CallError(response)) {
  let reply_subject = new_subject()

  // Monitor the callee process so we can tell if it goes down (meaning we
  // won't get a reply)
  let monitor = monitor_process(subject_owner(subject))

  // Send the request to the process over the channel
  send(subject, make_request(reply_subject))

  // Await a reply or handle failure modes (timeout, process down, etc)
  let result =
    new_selector()
    |> selecting(reply_subject, Ok)
    |> selecting_process_down(
      monitor,
      fn(down: ProcessDown) { Error(CalleeDown(reason: down.reason)) },
    )
    |> select(timeout)

  // Demonitor the process and close the channels as we're done
  demonitor_process(monitor)

  // Prepare an appropriate error (if present) for the caller
  case result {
    Error(Nil) -> Error(CallTimeout)
    Ok(res) -> res
  }
}

/// Send a message to a process and wait for a reply.
///
/// If the receiving process exits or does not reply within the allowed amount
/// of time the calling process crashes. If you wish an error to be returned
/// instead see the `try_call` function.
///
pub fn call(
  subject: Subject(request),
  make_request: fn(Subject(response)) -> request,
  within timeout: Int,
) -> response {
  assert Ok(resp) = try_call(subject, make_request, timeout)
  resp
}

/// Creates a link between the calling process and another process.
///
/// When a process crashes any linked processes will also crash. This is useful
/// to ensure that groups of processes that depend on each other all either
/// succeed or fail together.
///
/// Returns `True` if the link was created successfully, returns `False` if the
/// process was not alive and as such could not be linked.
///
pub external fn link(pid: Pid) -> Bool =
  "gleam_erlang_ffi" "link"

external fn erlang_unlink(pid: Pid) -> Bool =
  "erlang" "unlink"

/// Removes any existing link between the caller process and the target process.
///
pub fn unlink(pid: Pid) -> Nil {
  erlang_unlink(pid)
  Nil
}

pub external type Timer

external fn erlang_send_after(Int, Pid, msg) -> Timer =
  "erlang" "send_after"

/// Send a message over a channel after a specified number of milliseconds.
///
pub fn send_after(subject: Subject(msg), delay: Int, message: msg) -> Timer {
  erlang_send_after(delay, subject.owner, #(subject.tag, message))
}

external fn erlang_cancel_timer(Timer) -> Dynamic =
  "erlang" "cancel_timer"

/// Values returned when a timer is cancelled.
///
pub type Cancelled {
  /// The timer could not be found. It likely has already triggered.
  ///
  TimerNotFound

  /// The timer was found and cancelled before it triggered.
  ///
  /// The amount of remaining time before the timer was due to be triggered is
  /// returned in milliseconds.
  ///
  Cancelled(time_remaining: Int)
}

/// Cancel a given timer, causing it not to trigger if it has not done already.
///
pub fn cancel_timer(timer: Timer) -> Cancelled {
  case dynamic.int(erlang_cancel_timer(timer)) {
    Ok(i) -> Cancelled(i)
    Error(_) -> TimerNotFound
  }
}

type KillFlag {
  Kill
}

external fn erlang_kill(to: Pid, because: KillFlag) -> Bool =
  "erlang" "exit"

// Go, my pretties. Kill! Kill!
// - Bart Simpson
//
/// Send an untrappable `kill` exit signal to the target process.
///
/// See the documentation for the Erlang [`erlang:exit`][1] function for more
/// information.
///
/// [1]: https://erlang.org/doc/man/erlang.html#exit-1
///
pub fn kill(pid: Pid) -> Nil {
  erlang_kill(pid, Kill)
  Nil
}

external fn erlang_send_exit(to: Pid, because: whatever) -> Bool =
  "erlang" "exit"

// TODO: test
/// Sends an exit signal to a process, indicating that the process is to shut
/// down.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#exit-2
///
pub fn send_exit(to pid: Pid) -> Nil {
  erlang_send_exit(pid, Normal)
  Nil
}

/// Sends an exit signal to a process, indicating that the process is to shut
/// down due to an abnormal reason such as a failure.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#exit-2
///
pub fn send_abnormal_exit(to pid: Pid, reason: String) -> Nil {
  erlang_send_exit(pid, Abnormal(reason))
  Nil
}

/// Set whether the current process is to trap exit signals or not.
///
/// When not trapping exits if a linked process crashes the exit signal
/// propagates to the process which will also crash.
/// This is the normal behaviour before this function is called.
///
/// When trapping exits (after this function is called) if a linked process
/// crashes an exit message is sent to the process instead. These messages can
/// be handled with the `selecting_trapped_exits` function.
///
pub external fn trap_exits(Bool) -> Nil =
  "gleam_erlang_ffi" "trap_exits"
