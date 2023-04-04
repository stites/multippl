#!/usr/bin/env bash

for gridsize in 3 4 5 6; do
    for determinism in 0.0; do
        cargo run --release --bin plot-grids --features=plots -- \
              -v 1 \
              variance \
              --gridsize $gridsize \
              --comptype approx \
              --determinism $determinism \
              --runs 1000000
    done
done
