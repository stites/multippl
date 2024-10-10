#!/usr/bin/env bash
set +e #otherwise the script will exit on error. needed for set membership check

CLEAN=0
BENCH=0
BENCH_PSI=0
AVG=0
case $1 in
  clean)
    echo "clean)"
    CLEAN=1
    BENCH=0
    BENCH_PSI=0
    AVG=0
    ;;
  avg)
    echo "avg)"
    AVG=1
    BENCH=0
    BENCH_PSI=0
    CLEAN=0
    ;;

  bench)
    echo "bench)"
    AVG=1
    BENCH=1
    CLEAN=0
    shift
    while [[ "$1" =~ ^- && ! "$1" == "--" ]]; do case $1 in
      --clean) CLEAN=1 ;;
      --no-avg) AVG=0 ;;
      --no-psi)
        BENCH_PSI=0
        ;;
      --psi)
        BENCH=0
        BENCH_PSI=1
        ;;
    esac; shift; done
    if [[ "$1" == '--' ]]; then shift; fi
    ;;
  *)
    echo "please use one of ./run.sh clean|avg|bench"
esac

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

run_avgs() {
    (cd "$1" && python avg.py)
}
run_benchmark() {
    PSI_FLAG=""
    if [ "$2" == 1 ]; then
        PSI_FLAG="--psi"
    fi
    echo "with $NUM_THREADS threads"
    COMMAND="python ./bench.py --num-runs $NUM_RUNS --num-steps $NUM_STEPS --threads $NUM_THREADS $PSI_FLAG"
    echo "$COMMAND"
    (cd "$1" && eval "${COMMAND}" )
}
run_clean() {
    (cd "$1" && rm -rf logs/)
}

function run_phase {
  MODE="$1"
  PSIARG="$2"
  for exp in ./*; do
      if elementIn "$exp" "${SKIP[@]}"; then
          echo "skipping $exp"
      elif [[ "$exp" == *"__pycache__"* ]]; then
          continue
      else
          if [ -d "$exp" ]; then
              echo "$MODE" "$exp"
              $MODE "$exp" "$PSIARG"
          fi
      fi
  done
}
if [ $CLEAN = 1 ]; then
  run_phase "run_clean"
fi

if [ $BENCH = 1 ]; then
  run_phase "run_benchmark" 0
fi

if [ $BENCH_PSI = 1 ]; then
  run_phase "run_benchmark" 1
fi

if [ $AVG = 1 ]; then
  run_phase "run_avgs"
fi
