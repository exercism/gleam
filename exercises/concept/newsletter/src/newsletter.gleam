pub fn read_emails(path: String) -> Result(List(String), Nil) {
  todo
}

pub fn create_log_file(path: String) -> Result(Nil, Nil) {
  todo
}

pub fn log_sent_email(path: String, email: String) -> Result(Nil, Nil) {
  todo
}

pub fn send_newsletter(
  emails_path: String,
  log_path: String,
  send_email: fn(String) -> Result(Nil, Nil),
) -> Result(Nil, Nil) {
  todo
}
