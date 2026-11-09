# Exercism Gleam Track

[![Configlet Status](https://github.com/exercism/gleam/actions/workflows/configlet.yml/badge.svg)](https://github.com/exercism/gleam/actions/workflows/configlet.yml)

Exercism exercises in Gleam.

## Regenerating tests

The exercise generator can be used to regenerate test files.

```bash
# Set "$@" to the slugs
pushd exercises/practice
set -- *
popd

# Use the generator to rewrite test files.
pushd exercise_generator
for slug; do
  gleam run "${slug}" "$(curl https://raw.githubusercontent.com/exercism/problem-specifications/main/exercises/${slug}/canonical-data.json)"
done

# Format the test files.
popd
gleam format exercises/practice/*/test/*_test.gleam
```
