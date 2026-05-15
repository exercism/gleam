# Hints

## General

- Review regular expression patterns from the introduction. Remember, when creating the pattern from a string, you must escape some characters.
- Check out this website about regular expressions: [Regular-Expressions.info][website-regex-info].
- Check out this website about regular expressions: [Rex Egg - The world's most tyrannosauical regex tutorial][website-rexegg].
- Check out this website about regular expressions: [RegexOne - Learn Regular Expressions with simple, interactive exercises][website-regexone].
- Check out this website about regular expressions: [Regular Expressions 101 - an online regex sandbox][website-regex-101].
- Check out this website about regular expressions: [RegExr - an online regex sandbox][website-regexr].

## 1. Identify garbled log lines

- The [`regexp.from_string` function][from-string] can be used to compile a regular expression.
- The [`regexp.check` function][check] can be used to test whether a regular expression matches a string.
- Don't forget to escape characters that have special meaning in regular expressions.

## 2. Split the log line

- The [`regexp.split` function][split] can be used to split a string using a regular expression.
- Don't forget to escape characters that have special meaning in regular expressions.

## 3. Tag lines with user names

- The [`regexp.scan` function][scan] can be used used to capture parts of a string using a regular expression.
- Don't forget to escape characters that have special meaning in regular expressions.

[website-regex-info]: https://www.regular-expressions.info
[website-rexegg]: https://www.rexegg.com/
[website-regexone]: https://regexone.com/
[website-regex-101]: https://regex101.com/
[website-regexr]: https://regexr.com/
[from-string]: https://hexdocs.pm/gleam_regexp/gleam/rregexp.html#from_string
[check]: https://hexdocs.pm/gleam_regexp/gleam/regexp.html#check
[split]: https://hexdocs.pm/gleam_regexp/gleam/regexp.html#split
[scan]: https://hexdocs.pm/gleam_regexp/gleam/regexp.html#scan
