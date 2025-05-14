for i in $(eza -D); do
  # echo $i
  pushd $i
  sed -I ".bak" "s|exercism_test_runner.*$|exercism_test_runner = { path = \"/Users/kalema/repos/exercism/gleam-test-runner/runner\" }|" gleam.toml
  popd
  exit 0
done
