#!/bin/bash

base_url="https://raw.githubusercontent.com/wgsl-tooling-wg/wesl-testsuite/main/src/test-cases-json/"
files=("importCases.json" "importSyntaxCases.json" "ctsParseTests.json")

for file in "${files[@]}"; do
  curl -O "${base_url}${file}"
done
