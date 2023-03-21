#!/usr/bin/env bash

for gridsize in 3 6 9 12 15; do
    for comptype in "approx" "exact"; do
        for determinism in 0.75 0.5 0.25 0.0; do
            CSV="${comptype}-g${gridsize}-d${determinism}.csv"
            if cargo run --bin plot-grids --features=plots -- --gridsize $gridsize --comptype $comptype --determinism $determinism --csv "${CSV}"; then
                noti -o -m "${CSV} done"
            else
                noti -o -m "${CSV} restarting..."
                for i in 1 2 3 4 5; do
                    if cargo run --bin plot-grids --features=plots -- --gridsize $gridsize --comptype $comptype --determinism $determinism --csv "${CSV}" --runs 1; then
                        noti -o -m "${CSV} restart#${i} done"
                    else
                        noti -o -m "${CSV} restart#${i} failed!"
                    fi
                done
            fi
        done
    done
done
