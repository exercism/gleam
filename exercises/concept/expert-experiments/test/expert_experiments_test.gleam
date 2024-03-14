import exercism/should
import exercism/test_runner
import expert_experiments
import gleam/erlang/process
import gleam/list

pub fn main() {
  test_runner.main()
}

fn mutable_yielder(elements: List(t)) -> fn() -> t {
  let subject = process.new_subject()
  list.each(elements, fn(element) { process.send(subject, element) })
  fn() {
    case process.receive(subject, 0) {
      Ok(element) -> element
      Error(_) -> panic as "Callback called too many times"
    }
  }
}

fn logger() -> #(fn(String) -> Nil, fn() -> List(String)) {
  let subject = process.new_subject()
  let writer = fn(message) { process.send(subject, message) }
  let reader = fn() { read_all(subject, []) }
  #(writer, reader)
}

fn read_all(
  subject: process.Subject(String),
  read: List(String),
) -> List(String) {
  case process.receive(subject, 0) {
    Ok(message) -> read_all(subject, [message, ..read])
    Error(_) -> list.reverse(read)
  }
}

pub fn with_retry_pass_test() {
  let function = mutable_yielder([Ok("First"), Error("Second")])
  {
    use <- expert_experiments.with_retry
    function()
  }
  |> should.equal(Ok("First"))
}

pub fn with_retry_fail_fail_test() {
  let function = mutable_yielder([Error("First"), Error("Second")])
  {
    use <- expert_experiments.with_retry
    function()
  }
  |> should.equal(Error("Second"))
}

pub fn with_retry_fail_pass_test() {
  let function = mutable_yielder([Error("First"), Ok("Second")])
  {
    use <- expert_experiments.with_retry
    function()
  }
  |> should.equal(Ok("Second"))
}

pub fn record_timing_pass_test() {
  let #(writer, reader) = logger()
  {
    use <- expert_experiments.record_timing(fn() { writer("timer") })
    writer("experiment")
    Ok(0)
  }
  |> should.equal(Ok(0))

  reader()
  |> should.equal(["timer", "experiment", "timer"])
}

pub fn record_timing_fail_test() {
  let #(writer, reader) = logger()
  {
    use <- expert_experiments.record_timing(fn() { writer("timer") })
    writer("experiment")
    Error(Nil)
  }
  |> should.equal(Error(Nil))

  reader()
  |> should.equal(["timer", "experiment", "timer"])
}

pub fn run_experiment_fail_test() {
  {
    let setup = fn() { Error("Setup failed") }
    let action = fn(_) { panic as "Should not run action" }
    use _, _ <- expert_experiments.run_experiment("Experiment 1", setup, action)
    panic as "Should not run record"
  }
  |> should.equal(Error("Setup failed"))
}

pub fn run_experiment_pass_fail_test() {
  {
    let setup = fn() { Ok(1) }
    let action = fn(x) {
      should.equal(x, 1)
      Error("Action failed")
    }
    use _, _ <- expert_experiments.run_experiment("Experiment 1", setup, action)
    panic as "Should not run record"
  }
  |> should.equal(Error("Action failed"))
}

pub fn run_experiment_pass_pass_fail_test() {
  {
    let setup = fn() { Ok(1) }
    let action = fn(x) {
      should.equal(x, 1)
      Ok("2")
    }
    use x, y <- expert_experiments.run_experiment("Experiment 1", setup, action)
    should.equal(x, 1)
    should.equal(y, "2")
    Error("Record failed")
  }
  |> should.equal(Error("Record failed"))
}

pub fn run_experiment_pass_pass_pass_test() {
  {
    let setup = fn() { Ok(1) }
    let action = fn(x) {
      should.equal(x, 1)
      Ok("2")
    }
    use x, y <- expert_experiments.run_experiment("Experiment 1", setup, action)
    should.equal(x, 1)
    should.equal(y, "2")
    Ok("Success!")
  }
  |> should.equal(Ok(#("Experiment 1", "Success!")))
}
