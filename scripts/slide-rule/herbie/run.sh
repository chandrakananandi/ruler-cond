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

SEEDS="seeds.txt"
SEED_COUNT=10
THREADS=4
TIMEOUT=300

# prepare seeds
if [ -f "$SEEDS" ]; then
    echo "Found seeds file"
else
    echo "Created seeds file"
    shuf -i 1-65535 -n $SEED_COUNT | sort -n > $SEEDS
fi

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
    $HERBIE report                  \
            --threads $THREADS      \
            --timeout $TIMEOUT      \
            $BENCH                  \
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
