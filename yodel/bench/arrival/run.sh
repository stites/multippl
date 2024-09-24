#!/usr/bin/env bash
set +e #otherwise the script will exit on error
# from https://stackoverflow.com/a/8574392/1529734
elementIn () {
  local e match="$1"
  shift
  for e; do [[ "$e" == "$match" ]] && return 0; done
  return 1
}
# SKIP=("grids/9x9" "arrival/9x9" "gossip/g10" "gossip/g20")
SKIP[0]="run all things"
NUM_STEPS=1000
NUM_RUNS=100
NUM_THREADS="$(($(\grep -c ^processor /proc/cpuinfo) / 2))"
CLEAN=true
MODE=run_benchmark
#MODE=run_avgs
#MODE=run_clean

run_avgs() {
    (cd "$1" && python avg.py)
}
run_benchmark() {
    (cd "$1" && python ./bench.py --num-runs $NUM_RUNS --num-steps $NUM_STEPS --threads $NUM_THREADS && python avg.py)
}
run_clean() {
    (cd "$1" && rm -rf logs/)
}
for exp in ./*; do
    if elementIn "$exp" "${SKIP[@]}"; then
        echo "skipping $exp"
    elif [[ "$exp" == *"__pycache__"* ]]; then
        echo "skipping $exp"
    else
        if [ "$CLEAN" = true ]; then
            echo run_clean "$exp"
            run_clean "$exp"
        fi
        if [ -d "$exp" ]; then
            echo $MODE "$exp with $NUM_THREADS threads"
            $MODE "$exp"
        fi
    fi
done
