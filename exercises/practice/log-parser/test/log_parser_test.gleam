import exercism/should
import exercism/test_runner
import log_parser

pub fn main() {
  test_runner.main()
}

pub fn valid_line_debug_test() {
  let assert True = log_parser.is_valid_line("[DEBUG] response time 3ms")
}

pub fn valid_line_info_test() {
  let assert True = log_parser.is_valid_line("[INFO] the latest information")
}

pub fn valid_line_warning_test() {
  let assert True =
    log_parser.is_valid_line("[WARNING] something might be wrong")
}

pub fn valid_line_error_test() {
  let assert True =
    log_parser.is_valid_line("[ERROR] something really bad happened")
}

pub fn valid_line_unknown_level_test() {
  let assert False =
    log_parser.is_valid_line("[BOB] something really bad happened")
}

pub fn valid_line_no_level_test() {
  let assert False = log_parser.is_valid_line("something really bad happened")
}

pub fn valid_line_level_with_no_brackets_test() {
  let assert False =
    log_parser.is_valid_line("ERROR something really bad happened")
}

pub fn valid_line_lowercase_level_test() {
  let assert False =
    log_parser.is_valid_line("[error] something really bad happened")
}

pub fn split_line_test() {
  "[DEBUG] Attempt nr 2<=>[DEBUG] Attempt nr 3<-*~*->[ERROR] Failed to send SMS."
  |> log_parser.split_line
  |> should.equal([
    "[DEBUG] Attempt nr 2", "[DEBUG] Attempt nr 3",
    "[ERROR] Failed to send SMS.",
  ])
}

pub fn split_line_no_symbols_test() {
  "[INFO] Attempt nr 1<>[INFO] Attempt nr 2"
  |> log_parser.split_line
  |> should.equal(["[INFO] Attempt nr 1", "[INFO] Attempt nr 2"])
}

pub fn split_line_other_symbols_test() {
  "[INFO] Attempt nr 1<=!>[INFO] Attempt nr 2< >[INFO] Attempt nr 3"
  |> log_parser.split_line
  |> should.equal([
    "[INFO] Attempt nr 1<=!>[INFO] Attempt nr 2< >[INFO] Attempt nr 3",
  ])
}

pub fn split_line_no_brackets_test() {
  "[ERROR] Failed to send SMS**[ERROR] Invalid API key."
  |> log_parser.split_line
  |> should.equal(["[ERROR] Failed to send SMS**[ERROR] Invalid API key."])
}

pub fn tag_with_user_name_test() {
  "[WARN] User James123 has exceeded storage space"
  |> log_parser.tag_with_user_name
  |> should.equal(
    "[USER] James123 [WARN] User James123 has exceeded storage space",
  )
}

pub fn tag_with_user_name_no_user_test() {
  "[DEBUG] Process started"
  |> log_parser.tag_with_user_name
  |> should.equal("[DEBUG] Process started")
}

pub fn tag_with_user_name_spaces_around_user_test() {
  "[INFO] User   Bob9 reported post fxa3qa"
  |> log_parser.tag_with_user_name
  |> should.equal("[USER] Bob9 [INFO] User   Bob9 reported post fxa3qa")
}

pub fn tag_with_user_name_tabs_around_user_test() {
  "[ERROR] User\t!!!\tdoes not have a valid payment method"
  |> log_parser.tag_with_user_name
  |> should.equal(
    "[USER] !!! [ERROR] User\t!!!\tdoes not have a valid payment method",
  )
}

pub fn tag_with_user_name_newlines_arund_user_test() {
  "[DEBUG] Created User\nAlice908101\nat 14:02"
  |> log_parser.tag_with_user_name
  |> should.equal(
    "[USER] Alice908101 [DEBUG] Created User\nAlice908101\nat 14:02",
  )
}

pub fn tag_with_user_name_name_at_end_of_line_test() {
  "[INFO] New log in for User __JOHNNY__"
  |> log_parser.tag_with_user_name
  |> should.equal("[USER] __JOHNNY__ [INFO] New log in for User __JOHNNY__")
}

pub fn tag_with_user_name_name_unicode_name_test() {
  "[INFO] Promoted User АНАСТАСІЯ_🙂 to admin"
  |> log_parser.tag_with_user_name
  |> should.equal(
    "[USER] АНАСТАСІЯ_🙂 [INFO] Promoted User АНАСТАСІЯ_🙂 to admin",
  )
}
