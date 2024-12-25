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

case $1 in
  all)
    shift
    while [[ "$1" =~ ^- && ! "$1" == "--" ]]; do case $1 in
      --num-threads)     shift; NUM_THREADS=$1 ;;
      --num-runs)    shift; NUM_RUNS=$1    ;;
      --num-steps)   shift; NUM_STEPS=$1   ;;

      --psi-threads) shift; PSI_THREADS=$1 ;;
      --psi-runs)    shift; PSI_RUNS=$1    ;;

      --logdir)      shift; LOGDIR=$1      ;;
    esac; shift; done
    if [[ "$1" == '--' ]]; then shift; fi

    for exp in "${EXPERIMENTS[@]}"; do
        echo "$exp"
        ( (cd "$BENCH_DIR/$exp" && \
             python ./bench.py \
                 --threads   "$NUM_THREADS" \
                 --num-steps "$NUM_STEPS" \
                 --num-runs  "$NUM_RUNS" \
                 --logdir    "$LOGDIR/$exp" ) || exit 1)
    done
    echo "non-PSI evaluations complete. running PSI evaluations now."
    sleep 3

    for exp in "${EXPERIMENTS[@]}"; do
        echo "psi $exp"
        ( (cd "$BENCH_DIR/$exp" && \
             python ./bench.py --psi \
                 --threads   "$PSI_THREADS" \
                 --num-runs  "$PSI_RUNS" \
                 --logdir    "$LOGDIR/$exp" ) || exit 1)
    done
    echo "all evaluations complete. tabulating results now..."
    sleep 3

    python "$BENCH_DIR/tabulate.py" --logdir "$LOGDIR"

    ;;
  tabulate)
    python "$BENCH_DIR/tabulate.py" --logdir "$LOGDIR"
    exit 0
    ;;
  *)
    echo "multippl-benchmark (all|tabulate) [OPTIONS]"
    echo ""
    echo "subcommand: all -- run all benchmarks (psi benchmarks last), then tabulate"
    echo ""
    echo "    --num-threads NUM_THREADS Number of threads to use for non-psi benchmarks. Default: 1."
    echo "    --num-runs NUM_RUNS       Number of runs to use for non-psi benchmarks. Default: 100."
    echo "    --num-steps NUM_STEPS     Number of steps per run to use for non-psi, approximate benchmarks. Default: 1000."
    echo ""
    echo "    --psi-threads PSI_THREADS Number of threads to use for psi benchmarks. Default: 1."
    echo "    --psi-runs PSI_RUNS       Number of runs to use for psi benchmarks. Default: 100."
    echo ""
    echo "    --logdir LOGDIR           Directory to store execution logs. Defaults to <cwd>/logs."
    echo ""
    echo "subcommand: tabulate -- skip benchmarks and tabulate"
    echo "    --logdir LOGDIR           Directory to store execution logs. Defaults to <cwd>/logs."
    echo ""
    exit 0
    ;;
esac

