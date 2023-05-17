# Instructions

In this exercise you'll be writing code to keep track of a list of programming languages you want to learn on Exercism.

You have six tasks, which will all involve dealing with lists.

## 1. Create a new list

To keep track of the languages you want to learn, you'll need to create a new list. Define the `new_list` function that returns a new, empty list.

```gleam
new_list()
// -> []
```

## 2. Define an existing list

Currently, you have a piece of paper listing the languages you want to learn: Gleam, Go, and TypeScript. Define the `existing_list` function to return this list.

```gleam
existing_list()
// -> ["Gleam", "Go", "TypeScript"]
```

## 3. Add a new language to a list

As you explore Exercism and find more interesting languages, you want to add them to your list. Implement the `add_language` function to add a new language to the beginning of your list.

```gleam
add_language(["OCaml", "Elixir"], "Scheme")
// -> ["Scheme", "OCaml", "Elixir"]
```

## 4. Count the languages in the list

Counting the languages one-by-one is inconvenient. Implement the `count_languages` function to count the number of languages on your list.

```gleam
count_languages(["jq", "Elm", "Rust", "Kotlin"])
// -> 4
```

## 5. Reverse the list

At some point, you realize that your list is actually ordered backwards! Implement the `reverse_list` function to reverse your list.

```gleam
reverse_list(["Python", "Julia", "Idris", "COBOL"])
// -> ["COBOL", "Idris", "Julia", "Python"]
```

## 6. Check if list is exciting

While you love all languages, Gleam has a special place in your heart. As such, you're really excited about a list of languages if:

- The first on the list is Gleam.
- The second item on the list is Gleam and the list contain either two or three languages.

Implement the `exciting_list` function to check if a list of languages is exciting:

```gleam
exciting_list(["Lua", "Gleam"])
// -> true
```
