#!/usr/bin/env bash
set +e #otherwise the script will exit on error
# from https://stackoverflow.com/a/8574392/1529734
elementIn () {
  local e match="$1"
  shift
  for e; do [[ "$e" == "$match" ]] && return 0; done
  return 1
}
SKIP=("grids/9x9" "arrival/9x9")
NUM_STEPS=10
NUM_RUNS=2

run_benchmark() {
    #(cd "$1" && rm -rf logs/)
    (cd "$1" && python ./bench.py --num-runs $NUM_RUNS --num-steps $NUM_STEPS && python avg.py)
}

run_clean() {
    (cd "$1" && rm -rf logs/)
}


for exp_meta in grids arrival; do
    for exp in "$exp_meta"/*; do
        if elementIn "$exp" "${SKIP[@]}"; then
            echo "skipping $exp"
        else
            if [ -d "$exp" ]; then
                run_benchmark "$exp"
                #run_clean "$exp"
            fi
        fi
    done
done
