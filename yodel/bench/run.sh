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
# NUM_STEPS=1
# NUM_RUNS=2
SKIP[0]="test everything"
NUM_STEPS=1000
NUM_RUNS=10


run_benchmark() {
    (cd "$1" && python ./bench.py --num-runs $NUM_RUNS --num-steps $NUM_STEPS && python avg.py)
}

run_clean() {
    (cd "$1" && rm -rf logs/)
}


MODE=run_benchmark
MODE=run_clean

for exp_meta in grids arrival gossip; do
    for exp in "$exp_meta"/*; do
        if elementIn "$exp" "${SKIP[@]}"; then
            echo "skipping $exp"
        else
            if [ -d "$exp" ]; then
                echo $MODE "$exp"
                $MODE "$exp"
            fi
        fi
    done
done
