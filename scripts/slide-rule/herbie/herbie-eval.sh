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

# configuration
HERBIE_DIR=herbie/
HERBIE_COMMIT=e00a7c5018edd7eba037318330b1bbbd4fdc202a
OUTPUT_DIR=output/
BENCH_DIR=ruler-bench/

# Install Herbie
BUILD_DIR=$HERBIE_DIR HERBIE_COMMIT=$HERBIE_COMMIT ./install.sh

# Form benchmark suite
mkdir -p $BENCH_DIR
cp -r "$HERBIE_DIR/bench/hamming" $BENCH_DIR
cp -r "$HERBIE_DIR/bench/pbrt.fpcore" $BENCH_DIR

# ASSUMING rules under rulesets/
# Convert Ruler rules to Herbie format
racket ruler-to-herbie.rkt --tags arithmetic \
  empty-rules.rkt rulesets/slide-rule.json slide-rule-rules.rkt
racket ruler-to-herbie.rkt --tags arithmetic --old-format \
  empty-rules.rkt rulesets/oopsla21.json oopsla21-rules.rkt

# Run Herbie
HERBIE_CMD="racket $HERBIE_DIR/src/herbie.rkt"

mkdir -p $OUTPUT_DIR

# Run with current rules
echo "Running Pareto-Herbie with current ruleset"
cp default-rules.rkt $HERBIE_DIR/src/syntax/rules.rkt
HERBIE=$HERBIE_CMD OUTPUT_DIR="$OUTPUT_DIR/main" BENCH=$BENCH_DIR ./run.sh

# Run with SlideRule rules
echo "Running Pareto-Herbie with SlideRule ruleset"
cp slide-rule-rules.rkt $HERBIE_DIR/src/syntax/rules.rkt
HERBIE=$HERBIE_CMD OUTPUT_DIR="$OUTPUT_DIR/slide-rule" BENCH=$BENCH_DIR ./run.sh

# Run with OOPSLA21 rules
echo "Running Pareto-Herbie with OOPSLA 21 ruleset"
cp oopsla21-rules.rkt $HERBIE_DIR/src/syntax/rules.rkt
HERBIE=$HERBIE_CMD OUTPUT_DIR="$OUTPUT_DIR/oopsla21" BENCH=$BENCH_DIR ./run.sh

# Plot result
SEEDS="seeds.txt"

mkdir -p $OUTPUT_DIR/plot
while read -r seed; do
    python3 plot.py                   \
      "$OUTPUT_DIR/main/$seed"        \
      "$OUTPUT_DIR/slide-rule/$seed"  \
      "$OUTPUT_DIR/oopsla21/$seed"    \
      "$OUTPUT_DIR/plot/$seed.png"
done < $SEEDS
