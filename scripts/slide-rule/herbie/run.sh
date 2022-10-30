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
  echo "benchmark directory not provided"
  exit 1
else
  BENCH=$2
fi

# check for output directory
if [ -z "$3" ]; then
  echo "output directory not provided"
  exit 1
else
  OUTPUT_DIR=$3
fi

# check for herbie directory
if [ -z "$HERBIE_DIR" ]; then
  echo "herbie directory not provided"
  exit 1
fi

# set timeout
if [ -z "$TIMEOUT" ]; then
  TIMEOUT=300
fi

# set threads
if [ -z "$THREADS" ]; then
  THREADS=4
fi

echo "Timeout limit set at $TIMEOUT seconds"
echo "Running with $THREADS threads"

# run Herbie
mkdir -p $OUTPUT_DIR

# advise user of execution plan
if [ -z "$PARALLEL_SEEDS" ]; then
  echo "Using Herbie concurrency only."
else
  # support for exporting bash environment to parallel
  source $(which env_parallel.bash)
  env_parallel --record-env

  echo "Using multiple concurrent Herbie runs in parallel."
  echo "Restricting to $PARALLEL_SEEDS parallel concurrent Herbie runs."
fi

function do_seed {
  seed="$1"
  mkdir -p "$OUTPUT_DIR/$seed"
  racket "$HERBIE_DIR/src/herbie.rkt"   \
          report                        \
          --seed $seed                  \
          --threads $THREADS            \
          --timeout $TIMEOUT            \
          $BENCH                        \
          "$OUTPUT_DIR/$seed"
}

# sample herbie behavior
if [ -z "$PARALLEL_SEEDS" ]; then
  # by default, do not use parallel
  while read -r seed; do
    do_seed "$seed"
  done < $SEEDS
else
  # conditionally use parallel
  #
  # Note that Herbie can already use up to # of benchmarks cores,
  # so this probably only makes sense if you have PARALLEL_SEEDS
  # set to something less than # of cores divided by # of benchmarks,
  # i.e., you have a lot of cores. We're not at all careful to get
  # solid timing numbers, but going higher any than that will make
  # any time measurements even less meaningful.
  cat $SEEDS                      \
    | env_parallel                \
        --env _                   \
        --jobs "$PARALLEL_SEEDS"  \
        --halt now,fail=1         \
        do_seed
fi

# graph
