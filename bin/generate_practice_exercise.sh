#!/usr/bin/env bash

# Exit if anything fails.
set -euo pipefail

# If argument not provided, print usage and exit
if [ $# -ne 1 ]; then
    echo "Usage: bin/generate_practice_exercise.sh <exercise-slug>"
    exit 1
fi

SLUG="$1"
exercise_dir="exercises/practice/${SLUG}"

download() {
    file="$1"
    url="$2"
    curl --silent --show-error --fail --retry 3 --max-time 3 \
        --output "$file" "$url"
}

# build configlet
echo "Fetching latest version of configlet..."
./bin/fetch-configlet

# Preparing config.json
echo "Adding instructions and configuration files..."
UUID=$(bin/configlet uuid)
jq --arg slug "$SLUG" --arg uuid "$UUID" \
    '.exercises.practice += [{slug: $slug, name: "TODO", uuid: $uuid, practices: [], prerequisites: [], difficulty: 5}]' \
    config.json > config.json.tmp
# jq always rounds whole numbers, but average_run_time needs to be a float
sed -i -e 's/"average_run_time": \([[:digit:]]\+\)$/"average_run_time": \1.0/' config.json.tmp
mv config.json.tmp config.json

# Create instructions and config files
./bin/configlet sync --update --yes --docs --metadata --exercise "$SLUG"

packages_prefix="https://raw.githubusercontent.com/exercism/gleam-test-runner/main/packages"
download "$exercise_dir"/gleam.toml "$packages_prefix"/gleam.toml
download "$exercise_dir"/manifest.toml "$packages_prefix"/manifest.toml
./bin/configlet sync --update --yes --filepaths --exercise "$SLUG"

name=$(echo $SLUG | sed 's/-/_/g' )
sed -i -e "s/name = \".*\"/name = \"$name\"/" "$exercise_dir"/gleam.toml

cat <<EOT > "$exercise_dir"/.gitignore
*.beam
*.ez
build
erl_crash.dump
EOT

# Create Gleam files
echo "Creating Gleam files..."
mkdir -p ${exercise_dir}/test ${exercise_dir}/src

pushd exercise_generator
gleam run $SLUG "$(curl https://raw.githubusercontent.com/exercism/problem-specifications/main/exercises/${SLUG}/canonical-data.json)"
popd
gleam format ${exercise_dir}/{.meta,src,test}/*.gleam

echo "All stub files were created. After implementing the solution, tests and configuration, please run:"
echo "    ./bin/configlet sync --update --tests --exercise ${SLUG}"
echo "    ./bin/configlet fmt --update --yes --exercise ${SLUG}"
