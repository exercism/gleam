import exercism/should
import exercism/test_runner
import newsletter
import simplifile

pub fn main() {
  test_runner.main()
}

pub fn read_emails_test() {
  let emails =
    "lucy@example.com
thomas@example.com
sid@example.com
"
  let assert Ok(_) = simplifile.write("emails.txt", emails)
  let assert Ok(emails) = newsletter.read_emails("emails.txt")
  emails
  |> should.equal(["lucy@example.com", "thomas@example.com", "sid@example.com"])
}

pub fn create_log_file_test() {
  let _ = simplifile.delete("log.txt")
  let assert Ok(Nil) = newsletter.create_log_file("log.txt")
  let assert Ok(log) = simplifile.read("log.txt")
  log
  |> should.equal("")
}

pub fn log_sent_email_test() {
  let _ = simplifile.delete("log.txt")
  let assert Ok(Nil) = newsletter.create_log_file("log.txt")
  let assert Ok(Nil) =
    newsletter.log_sent_email("log.txt", "janice@example.com")

  let assert Ok(log) = simplifile.read("log.txt")
  log
  |> should.equal("janice@example.com\n")

  let assert Ok(Nil) = newsletter.log_sent_email("log.txt", "joe@example.com")
  let assert Ok(log) = simplifile.read("log.txt")
  log
  |> should.equal("janice@example.com\njoe@example.com\n")
}

pub fn send_newsletter_test() {
  let _ = simplifile.delete("log.txt")
  let emails =
    "bushra@example.com
abdi@example.com
bell@example.com
"
  let assert Ok(Nil) = simplifile.write("emails.txt", emails)

  let send_email = fn(email) {
    case email {
      "bushra@example.com" -> {
        let assert Ok(log) = simplifile.read("log.txt")
        log
        |> should.equal("")
        Ok(Nil)
      }
      "abdi@example.com" -> {
        let assert Ok(log) = simplifile.read("log.txt")
        log
        |> should.equal("bushra@example.com\n")
        Error(Nil)
      }
      "bell@example.com" -> {
        let assert Ok(log) = simplifile.read("log.txt")
        log
        |> should.equal("bushra@example.com\n")
        Ok(Nil)
      }
      _ -> panic as "Unexpected email given to send_email function"
    }
  }

  let assert Ok(Nil) =
    newsletter.send_newsletter("emails.txt", "log.txt", send_email)

  let assert Ok(log) = simplifile.read("log.txt")
  log
  |> should.equal("bushra@example.com\nbell@example.com\n")
}
