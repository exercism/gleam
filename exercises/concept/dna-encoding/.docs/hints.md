# Hints

## General

- The `0b00` syntax can be used to specify an int in binary syntax rather than
  decimal syntax.

## 1. Encode nucleic acid to binary value

- The nucleotide type can be pattern matched on with a case expression.

## 2. Decode the binary value to the nucleic acid

- The binary int syntax can be used in patterns.
- Return `Error(Nil)` if the int is is not a valid nucleotide.

## 3. Encode a DNA list

- The `encode_nucleotide` function can be used to encode a single nucleotide.

## 4. Decode a DNA bitarray

- The binary int syntax can be used within a bit array pattern.
- Return `Error(Nil)` if the remaining bit array is not long enough to contain
  a nucleotide.
