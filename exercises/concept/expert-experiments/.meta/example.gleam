import gleam/result

pub fn with_retry(experiment: fn() -> Result(t, e)) -> Result(t, e) {
  experiment()
  |> result.lazy_or(experiment)
}

pub fn record_timing(
  time_logger: fn() -> Nil,
  experiment: fn() -> Result(t, e),
) -> Result(t, e) {
  time_logger()
  let result = experiment()
  time_logger()
  result
}

pub fn run_experiment(
  name: String,
  setup: fn() -> Result(t, e),
  action: fn(t) -> Result(u, e),
  record: fn(t, u) -> Result(v, e),
) -> Result(#(String, v), e) {
  use setup_data <- result.try(setup())
  use action_data <- result.try(action(setup_data))
  use record_data <- result.try(record(setup_data, action_data))
  Ok(#(name, record_data))
}
