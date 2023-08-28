# Instructions

Boris is a big model train enthusiast and has decided to share their passion with the world by starting a newsletter. They'll start by sending the first issue of the newsletter to friends and acquaintances that share the hobby, their email addresses stored in a text file.

## 1. Read email addresses from a file

Implement the `read_emails` function. It takes a path string to a text file that contains email addresses separated by newlines, and returns a list of the email addresses from the file.

```gleam
read_emails("/home/my_user/documents/model_train_friends_emails.txt")
// -> Ok(["rick@example.com", "choochoo42@example.com", "anna@example.com"])
```

## 2. Create a log file for writing

Sending an email is a task that might fail for many unpredictable reasons, like a typo in the email address or temporary network issues. To ensure that you can retry sending the emails to all your friends without sending duplicates, you need to log the email addresses that already received the email. For this, you'll need a log file.

Implement the `create_log_file` function. It takes a file path and creates a new empty file at that location.

```gleam
create_log_file("/home/my_user/documents/newsletter_issue1_log.txt")
// -> Ok(Nil)
```

## 3. Log a sent email

Implement the `log_sent_email` function. It takes a path to a log file and a string with the email address. It writes the email address to the file, followed by a newline.

```gleam
log_sent_email(
  "/home/my_user/documents/newsletter_issue1_log.txt",
  "joe@example.com",
)
// -> Ok(Nil)
```

## 4. Send the newsletter

Now that you have all of the building blocks of the email sending procedure, you need to combine them together in a single function.

Implement the `send_newsletter` function. It takes a path of the file with email addresses, a path of a log file, and an anonymous function that sends an email to a given email address.

It should read all the email addresses from the given file and attempt to send an email to every one of them. If the anonymous function that sends the email returns an `Ok` value, write the email address to the log file. Make sure to do it as soon as the email is sent.

```gleam
send_newsletter(
  "model_train_friends_emails.txt",
  "newsletter_issue1_log.txt",
  send_email,
)
// -> Ok(Nil)
```
