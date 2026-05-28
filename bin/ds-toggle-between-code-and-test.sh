#!/usr/bin/env bash
set -euo pipefail

path="${1:?missing path}"

if [[ "$path" == test/*_test.exs ]]; then
  # test/foo/bar_test.exs -> lib/foo/bar.ex
  result="$path"

  result="${result#test/}"
  result="lib/${result%_test.exs}.ex"

  echo "$result"

elif [[ "$path" == lib/*.ex ]]; then
  # lib/foo/bar.ex -> test/foo/bar_test.exs
  result="$path"

  result="${result#lib/}"
  result="test/${result%.ex}_test.exs"

  echo "$result"

else
  echo "unsupported path: $path" >&2
  exit 1
fi
