#!/usr/bin/env bash
set +e #otherwise the script will exit on error. needed for set membership check

# SKIP=("grids/9x9" "arrival/9x9" "gossip/g10" "gossip/g20")
SKIP[0]="run all things"
NUM_STEPS=1000
NUM_RUNS=100
TIMEOUT_MIN=30
NUM_THREADS="$(($(\grep -c ^processor /proc/cpuinfo) / 2))"
LOGDIR="logs/"

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
      --threads) shift; NUM_THREADS=$1 ;;
      --timeout-min) shift; TIMEOUT_MIN=$1 ;;
      --runs) shift; NUM_RUNS=$1 ;;
      --steps) shift; NUM_STEPS=$1 ;;
      --logdir) shift; LOGDIR=$1 ;;
      --no-psi) BENCH_PSI=0 ;;
      --psi)
        BENCH=0
        BENCH_PSI=1
        ;;
    esac; shift; done
    if [[ "$1" == '--' ]]; then shift; fi
    ;;
  *)
    echo "./run.sh [clean|avg|bench]"
    echo "bench options:"
    echo "  --logdir      set the log directory for cached content."
    echo "  --clean       also clean the logdir"
    echo "  --no-avg      don't run avg.py"
    echo "  --threads     # threads to use for benchmarking. Default: half of available via nprocs"
    echo "  --timeout-min # minutes before a timeout. Default 30"
    echo "  --runs        # of runs to average over. Default: 100"
    echo "  --steps       # of steps per run. Default: 1000"
    echo "  --[no-]psi    include psi in the evaluation: Default: --no-psi"
    exit 1
esac

# from https://stackoverflow.com/a/8574392/1529734
elementIn () {
  local e match="$1"
  shift
  for e; do [[ "$e" == "$match" ]] && return 0; done
  return 1
}

run_avgs() {
    (cd "$1" && python ./avg.py)
}
run_benchmark() {
    PSI_FLAG=""
    if [ "$2" == 1 ]; then
        PSI_FLAG="--psi"
    fi
    echo "with $NUM_THREADS threads"
    COMMAND="python ./bench.py --num-runs $NUM_RUNS --timeout-min $TIMEOUT_MIN --num-steps $NUM_STEPS --logdir $LOGDIR --threads $NUM_THREADS $PSI_FLAG"
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
