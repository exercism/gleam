import exercism/should
import exercism/test_runner
import log_levels

pub fn main() {
  test_runner.main()
}

pub fn error_message_test() {
  log_levels.message("[ERROR]: Stack overflow")
  |> should.equal("Stack overflow")
}

pub fn warning_message_test() {
  log_levels.message("[WARNING]: Disk almost full")
  |> should.equal("Disk almost full")
}

pub fn info_message_test() {
  log_levels.message("[INFO]: File moved")
  |> should.equal("File moved")
}

pub fn message_with_leading_and_trailing_white_space_test() {
  log_levels.message("[WARNING]:   \tTimezone not set  \r\n")
  |> should.equal("Timezone not set")
}

pub fn error_log_level_test() {
  log_levels.log_level("[ERROR]: Disk full")
  |> should.equal("error")
}

pub fn warning_log_level_test() {
  log_levels.log_level("[WARNING]: Unsafe password")
  |> should.equal("warning")
}

pub fn info_log_level_test() {
  log_levels.log_level("[INFO]: Timezone changed")
  |> should.equal("info")
}

pub fn error_reformat_test() {
  log_levels.reformat("[ERROR]: Segmentation fault")
  |> should.equal("Segmentation fault (error)")
}

pub fn warning_reformat_test() {
  log_levels.reformat("[WARNING]: Decreased performance")
  |> should.equal("Decreased performance (warning)")
}

pub fn info_reformat_test() {
  log_levels.reformat("[INFO]: Disk defragmented")
  |> should.equal("Disk defragmented (info)")
}

pub fn reformat_with_leading_and_trailing_white_space_test() {
  log_levels.reformat("[ERROR]: \t Corrupt disk\t \t \r\n")
  |> should.equal("Corrupt disk (error)")
}
