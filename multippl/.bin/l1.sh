#!/usr/bin/env bash

for gridsize in 3 4 5; do
    # shellcheck disable=SC2043
    for determinism in 0.0; do
        if cargo run --release --bin plot-grids --features=plots -- \
              -v 1 \
              variance \
              --overwrite-csv \
              --gridsize $gridsize \
              --comptype approx \
              --determinism $determinism \
              --steps 1000000 \
              --runs 10; then
            noti -o -m "${gridsize} ${determinism} done"
         fi
            noti -o -m "${gridsize} ${determinism} failed"
    done
done
