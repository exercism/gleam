import atbash_cipher
import exercism/should
import exercism/test_runner

pub fn main() {
  test_runner.main()
}

pub fn encode_encode_yes_test() {
  atbash_cipher.encode("yes")
  |> should.equal("bvh")
}

pub fn encode_encode_no_test() {
  atbash_cipher.encode("no")
  |> should.equal("ml")
}

pub fn encode_encode_omg_test() {
  atbash_cipher.encode("OMG")
  |> should.equal("lnt")
}

pub fn encode_encode_spaces_test() {
  atbash_cipher.encode("O M G")
  |> should.equal("lnt")
}

pub fn encode_encode_mindblowingly_test() {
  atbash_cipher.encode("mindblowingly")
  |> should.equal("nrmwy oldrm tob")
}

pub fn encode_encode_numbers_test() {
  atbash_cipher.encode("Testing,1 2 3, testing.")
  |> should.equal("gvhgr mt123 gvhgr mt")
}

pub fn encode_encode_deep_thought_test() {
  atbash_cipher.encode("Truth is fiction.")
  |> should.equal("gifgs rhurx grlm")
}

pub fn encode_encode_all_the_letters_test() {
  atbash_cipher.encode("The quick brown fox jumps over the lazy dog.")
  |> should.equal("gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt")
}

pub fn decode_decode_exercism_test() {
  atbash_cipher.decode("vcvix rhn")
  |> should.equal("exercism")
}

pub fn decode_decode_a_sentence_test() {
  atbash_cipher.decode("zmlyh gzxov rhlug vmzhg vkkrm thglm v")
  |> should.equal("anobstacleisoftenasteppingstone")
}

pub fn decode_decode_numbers_test() {
  atbash_cipher.decode("gvhgr mt123 gvhgr mt")
  |> should.equal("testing123testing")
}

pub fn decode_decode_all_the_letters_test() {
  atbash_cipher.decode("gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt")
  |> should.equal("thequickbrownfoxjumpsoverthelazydog")
}

pub fn decode_decode_with_too_many_spaces_test() {
  atbash_cipher.decode("vc vix    r hn")
  |> should.equal("exercism")
}

pub fn decode_decode_with_no_spaces_test() {
  atbash_cipher.decode("zmlyhgzxovrhlugvmzhgvkkrmthglmv")
  |> should.equal("anobstacleisoftenasteppingstone")
}
