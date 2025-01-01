#!/usr/bin/env bash
WORKING_DIR=$(pwd)
NUM_THREADS=1
BENCH_DIR=bench/
declare -a EXPERIMENTS=(grids/3x3 grids/6x6 grids/9x9 arrival/tree-15 arrival/tree-31 arrival/tree-63 gossip/g4 gossip/g10 gossip/g20 bayesnets/insurance bayesnets/alarm)

NUM_THREADS=1
NUM_RUNS=100
NUM_STEPS=1000

PSI_THREADS=1
PSI_RUNS=100

LOGDIR=$WORKING_DIR/logs
TIMEOUT_MIN=30

# get top-level action
ACTION=$1
RUN_PSI=1
shift
function help {
    echo "multippl-benchmark (all|tabulate) [OPTIONS]"
    echo ""
    echo "subcommand: all -- run all benchmarks (psi benchmarks last), then tabulate"
    echo ""
    echo "    --num-threads NUM_THREADS Number of threads to use for non-psi benchmarks. Default: 1."
    echo "    --num-runs NUM_RUNS       Number of runs to use for non-psi benchmarks. Default: 100."
    echo "    --num-steps NUM_STEPS     Number of steps per run to use for non-psi, approximate benchmarks. Default: 1000."
    echo ""
    echo "    --psi-threads PSI_THREADS Number of threads to use for PSI benchmarks. Default: 1."
    echo "    --psi-runs PSI_RUNS       Number of runs to use for PSI benchmarks. Default: 100."
    echo "    --no-psi                  Skip PSI benchmarks."
    echo ""
    echo "    --timeout-min TIMEOUT_MIN Number of minutes before a timeout. Default 30."
    echo "    --logdir LOGDIR           Directory to store execution logs. Defaults to <cwd>/logs."
    echo ""
    echo "subcommand: tabulate -- skip benchmarks and tabulate"
    echo "    --logdir LOGDIR           Directory to store execution logs. Defaults to <cwd>/logs."
    echo ""

}
# get command line args
while [[ "$1" =~ ^- && ! "$1" == "--" ]]; do case $1 in
  --num-threads)     shift; NUM_THREADS=$1 ;;
  --num-runs)    shift; NUM_RUNS=$1    ;;
  --num-steps)   shift; NUM_STEPS=$1   ;;

  --psi-threads) shift; PSI_THREADS=$1 ;;
  --psi-runs)    shift; PSI_RUNS=$1    ;;
  --no-psi)      shift; RUN_PSI=0       ;;

  --logdir)      shift; LOGDIR=$1      ;;
  --timeout-min) shift; TIMEOUT_MIN=$1 ;;
  --help|-h)
    help
    exit 0
    ;;
  *)
    echo "Saw unexpected flag: $1"
    echo "Run --help|-h for options"
    exit 1
    ;;
esac; shift; done
if [[ "$1" == '--' ]]; then shift; fi

if [ ! -d "${LOGDIR:0:1}" ]; then
  echo "top-level benchmarking requires an absolute log directory. Saw: $LOGDIR"
  exit 1
fi


if [ ! -d "$LOGDIR" ]; then
  mkdir -p "$LOGDIR"
fi

if ! test -w "$LOGDIR"; then
  echo "no permissions to write to $LOGDIR, likely logs were generated via"
  echo "docker. Please correct this: chmod -R a+w $LOGDIR"
  exit 1
fi
case $ACTION in
  all)
    for exp in "${EXPERIMENTS[@]}"; do
        echo "$exp"
        ( (cd "$BENCH_DIR/$exp" && \
             python ./bench.py \
                 --threads     "$NUM_THREADS" \
                 --num-steps   "$NUM_STEPS" \
                 --num-runs    "$NUM_RUNS" \
                 --timeout-min "$TIMEOUT_MIN" \
                 --logdir      "$LOGDIR" && \
             python ./avg.py --logdir "$LOGDIR"
          ) || exit 1)
    done
    if [ $RUN_PSI -eq 1 ]; then
        echo "non-PSI evaluations complete. running PSI evaluations now."
        sleep 3

        for exp in "${EXPERIMENTS[@]}"; do
            echo "psi $exp"
            ( (cd "$BENCH_DIR/$exp" && \
                 python ./bench.py --psi \
                     --threads     "$PSI_THREADS" \
                     --num-runs    "$PSI_RUNS" \
                     --timeout-min "$TIMEOUT_MIN" \
                     --logdir      "$LOGDIR" && \
                 python ./avg.py --logdir "$LOGDIR") || exit 1)
        done
    else
        echo "Saw --no-psi, not running PSI benchmark."
        sleep 3
    fi

    echo "Evaluations complete. tabulating results now..."
    sleep 3

    python "$BENCH_DIR/tabulate.py" --logdir "$LOGDIR"

    ;;
  tabulate)
    python "$BENCH_DIR/tabulate.py" --logdir "$LOGDIR"
    exit 0
    ;;
  *)
    help
    exit 0
    ;;
esac

