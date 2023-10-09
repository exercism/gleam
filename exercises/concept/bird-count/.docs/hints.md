# Hints

## General

- Try to split a problem into a base case and a recursive case. For example, let's say you want to count how many cookies are there in the cookie jar with a recursive approach. A base case is an empty jar - it has zero cookies. If the jar is not empty, then the number of cookies in the jar is equal to one cookie plus the number of cookies in the jar after removing one cookie.

## 1. Check how many birds visited today

- This task doesn't need recursion.
- Accessing the first element in a list can be done using a case expression.

## 2. Increment today's count

- This task doesn't need recursion.
- Accessing the first element in a list and the rest of the list can be done using a case expression.

## 3. Check if there was a day with no visiting birds

- Use recursion to iterate over elements in the list until a day with no visiting birds is found.
- The base case is an empty list.

## 4. Calculate the total number of visiting birds

- Use recursion to iterate over every element in the list.
- The base case is an empty list.

## 5. Calculate the number of busy days

- Use recursion to iterate over every element in the list.
- The base case is an empty list.
- Use a guard for one of the case expression clauses.
