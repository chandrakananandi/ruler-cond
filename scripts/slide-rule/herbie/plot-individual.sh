#!/usr/bin/env bash

# exit immediately upon first error
set -e

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

# check for seeds
if [ -z "$1" ]; then
  echo "seeds file not provided"
  exit 1
else
  SEEDS=$1
fi

# check for bench directory
if [ -z "$2" ]; then
  echo "output directory not provided"
  exit 1
else
  OUTPUT_DIR=$2
fi

mkdir -p $OUTPUT_DIR/plot
while read -r seed; do
  python3 plot.py                   \
    "$OUTPUT_DIR/main/$seed"        \
    "$OUTPUT_DIR/slide-rule/$seed"  \
    "$OUTPUT_DIR/oopsla21/$seed"    \
    "$OUTPUT_DIR/plot/$seed.png"
done < $SEEDS
