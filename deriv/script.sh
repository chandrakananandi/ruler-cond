#!/bin/bash
set -o errexit -o xtrace
touch derivability.txt

echo "Derivability of int using atoms 5, iters 2: " >> derivability.txt
cargo run --release --bin bigint derive int-iters2.json int-atoms5.json int-atoms5iters2.txt >> derivability.txt
echo "Derivability of int using lists 2, iters 2: " >> derivability.txt
cargo run --release --bin bigint derive int-iters2.json int-lists2.json int-lists2iters2.txt >> derivability.txt

echo "Derivability of bool using atoms 5, iters 4: " >> derivability.txt
cargo run --release --bin bool derive bool-iters4.json bool-atoms5.json bool-atoms5iters4.txt >> derivability.txt
echo "Derivability of bool using lists 2, iters 4: " >> derivability.txt
cargo run --release --bin bool derive bool-iters4.json bool-lists2.json bool-lists2iters4.txt >> derivability.txt
echo "Derivability of bool using lists 2, iters 2: " >> derivability.txt
cargo run --release --bin bool derive bool-iters2.json bool-lists2.json bool-lists2iters2.txt >> derivability.txt

echo "Derivability of rational using atoms 5, iters 2: " >> derivability.txt
cargo run --release --bin rational derive rational-iters2.json rational-atoms5.json rational-atoms5iters2.txt >> derivability.txt
echo "Derivability of rational using lists 1, iters 2: " >> derivability.txt
cargo run --release --bin rational derive rational-iters2.json rational-lists1.json rational-lists1iters2.txt >> derivability.txt
echo "Derivability of rational using lists 1, iters 1: " >> derivability.txt
cargo run --release --bin rational derive rational-iters1.json rational-lists1.json rational-lists1iters1.txt >> derivability.txt