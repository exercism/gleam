import gleeunit
import gleeunit/should
import affine_cipher.{KeyNotCoprime}

pub fn main() {
  gleeunit.main()
}

pub fn encode_yes_test() {
  affine_cipher.encode(plaintext: "yes", a: 5, b: 7)
  |> should.equal(Ok("xbt"))
}

pub fn encode_no_test() {
  affine_cipher.encode(plaintext: "no", a: 15, b: 18)
  |> should.equal(Ok("fu"))
}

pub fn encode_omg_test() {
  affine_cipher.encode(plaintext: "OMG", a: 21, b: 3)
  |> should.equal(Ok("lvz"))
}

pub fn encode_o_m_g_test() {
  affine_cipher.encode(plaintext: "O M G", a: 25, b: 47)
  |> should.equal(Ok("hjp"))
}

pub fn encode_mindblowingly_test() {
  affine_cipher.encode(plaintext: "mindblowingly", a: 11, b: 15)
  |> should.equal(Ok("rzcwa gnxzc dgt"))
}

pub fn encode_numbers_test() {
  affine_cipher.encode(plaintext: "Testing,1 2 3, testing.", a: 3, b: 4)
  |> should.equal(Ok("jqgjc rw123 jqgjc rw"))
}

pub fn encode_deep_thought_test() {
  affine_cipher.encode(plaintext: "Truth is fiction.", a: 5, b: 17)
  |> should.equal(Ok("iynia fdqfb ifje"))
}

pub fn encode_all_the_letters_test() {
  affine_cipher.encode(
    plaintext: "The quick brown fox jumps over the lazy dog.",
    a: 17,
    b: 33,
  )
  |> should.equal(Ok("swxtj npvyk lruol iejdc blaxk swxmh qzglf"))
}

pub fn encode_with_a_not_coprime_to_m_test() {
  affine_cipher.encode(plaintext: "This is a test.", a: 6, b: 17)
  |> should.equal(Error(KeyNotCoprime(6, 26)))
}

pub fn decode_exercism_test() {
  affine_cipher.decode(ciphertext: "tytgn fjr", a: 3, b: 7)
  |> should.equal(Ok("exercism"))
}

pub fn decode_a_sentence_test() {
  affine_cipher.decode(
    ciphertext: "qdwju nqcro muwhn odqun oppmd aunwd o",
    a: 19,
    b: 16,
  )
  |> should.equal(Ok("anobstacleisoftenasteppingstone"))
}

pub fn decode_numbers_test() {
  affine_cipher.decode(ciphertext: "odpoz ub123 odpoz ub", a: 25, b: 7)
  |> should.equal(Ok("testing123testing"))
}

pub fn decode_all_the_letters_test() {
  affine_cipher.decode(
    ciphertext: "swxtj npvyk lruol iejdc blaxk swxmh qzglf",
    a: 17,
    b: 33,
  )
  |> should.equal(Ok("thequickbrownfoxjumpsoverthelazydog"))
}

pub fn decode_with_no_spaces_in_input_test() {
  affine_cipher.decode(
    ciphertext: "swxtjnpvyklruoliejdcblaxkswxmhqzglf",
    a: 17,
    b: 33,
  )
  |> should.equal(Ok("thequickbrownfoxjumpsoverthelazydog"))
}

pub fn decode_with_too_many_spaces_test() {
  affine_cipher.decode(ciphertext: "vszzm    cly   yd cg    qdp", a: 15, b: 16)
  |> should.equal(Ok("jollygreengiant"))
}

pub fn decode_with_a_not_coprime_to_m_test() {
  affine_cipher.decode(ciphertext: "Test", a: 13, b: 5)
  |> should.equal(Error(KeyNotCoprime(13, 26)))
}
