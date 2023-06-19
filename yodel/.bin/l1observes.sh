#!/usr/bin/env bash

for gridsize in 3 4; do
    for clausesize in 2 3; do # this will never terminate, haha...
        for determinism in 0.0; do
            if cargo run --release --bin plot-grids --features=plots -- \
                  -v 1 \
                  --path "out/obs/$gridsize-$determinism-$clausesize/" \
                  variance \
                  --sliding-obs $clausesize \
                  --overwrite-csv \
                  --gridsize $gridsize \
                  --comptype approx \
                  --determinism $determinism \
                  --steps 1000000 \
                  --runs 3; then
                noti -o -m "${gridsize} ${determinism} done"
             fi
                noti -o -m "${gridsize} ${determinism} failed"
        done
    done
done
