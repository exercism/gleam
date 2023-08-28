# Hints

## 1. Read email addresses from a file

- The [`simplifile.read` function][file-read] can be used to read the contents of a file.

## 2. Create a log file for writing

- The [`simplifile.create_file` function][file-create] can be used to create an empty file.

## 3. Log a sent email

- The [`simplifile.append` function][file-append] can be used to append text to a file.

## 5. Send the newsletter

- All the necessary operations on files were already implemented in the previous steps.
- The [`result.try` function][result-try] and [`list.try_each` function][list-try-each] can be used to work with the `Result` type.

[file-read]: https://hexdocs.pm/simplifile/simplifile.html#read
[file-create]: https://hexdocs.pm/simplifile/simplifile.html#create_file
[file-append]: https://hexdocs.pm/simplifile/simplifile.html#append
[result-try]: https://hexdocs.pm/gleam_stdlib/gleam/result.html#try
[list-try-each]: https://hexdocs.pm/gleam_stdlib/gleam/list.html#try_each
