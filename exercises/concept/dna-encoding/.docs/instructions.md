# Instructions

In your DNA research lab, you have been working through various ways to compress your research data to save storage space. One teammate suggests converting the DNA data to a binary representation:

| Nucleic Acid | Code  |
| ------------ | ----- |
| Adenine      |  `00` |
| Cytosine     |  `01` |
| Guanine      |  `10` |
| Thymine      |  `11` |

You ponder this, as it will potentially reduce the required data storage costs, but at the expense of human readability. You decide to write a module to encode and decode your data to benchmark your savings.

## 1. Encode nucleic acid to binary value

Implement `encode_nucleotide` to accept a nucleotide and return the int value of the encoded code.

```gleam
encode_nucleotide(Cytosine)
// -> 1
// (which is equal to 0b01)
```

## 2. Decode the binary value to the nucleic acid

Implement `decode_nucleotide` to accept the integer value of the encoded code and return the nucleotide.

```gleam
decode_nucleotide(0b01)
// -> Ok(Cytosine)
```

## 3. Encode a DNA list

Implement `encode` to accept a list of nucleotides and return a bit string of the encoded data.

```elixir
encode([Adenine, Cytosine, Guanine, Thymine])
// -> <<27>>
```

## 4. Decode a DNA bitstring

Implement `decode` to accept a bit string representing nucleic acid and return the decoded data as a charlist.

```gleam
decode(<<27>>)
// -> Ok([Adenine, Cytosine, Guanine, Thymine])
```
