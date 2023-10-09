# Instructions

In this exercise, you are going to help high school sweethearts profess their love on social media by generating an ASCII heart with their initials:

```
     ******       ******
   **      **   **      **
 **         ** **         **
**            *            **
**                         **
**     J. K.  +  M. B.     **
 **                       **
   **                   **
     **               **
       **           **
         **       **
           **   **
             ***
              *
```

## 1. Get the name's first letter

Implement the `first_letter` function. It should take a name and return its first letter. It should clean up any unnecessary whitespace from the name.

```gleam
first_letter("Jane")
// -> "J"
```

## 2. Format the first letter as an initial

Implement the `initial` function. It should take a name and return its first letter, uppercase, followed by a dot. Make sure to reuse `first_letter` that you defined in the previous step.

```gleam
initial("Robert")
// -> "R."
```

## 3. Split the full name into the first name and the last name

Implement the `initials` function. It should take a full name, consisting of a first name and a last name separated by a space, and return the initials. Make sure to reuse `initial` that you defined in the previous step.

```gleam
initials("Lance Green")
// -> "L. G."
```

## 4. Put the initials inside of the heart

Implement the `pair/2` function. It should take two full names and return the initials inside an ASCII heart. Make sure to reuse `initials` that you defined in the previous step.

```gleam
pair("Blake Miller", "Riley Lewis")
// -> "
//      ******       ******
//    **      **   **      **
//  **         ** **         **
// **            *            **
// **                         **
// **     B. M.  +  R. L.     **
//  **                       **
//    **                   **
//      **               **
//        **           **
//          **       **
//            **   **
//              ***
//               *
// "
```
