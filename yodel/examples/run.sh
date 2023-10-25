#!/usr/bin/env bash
for f in arrival3x3.yo arrival6x6.yo arrival9x9.yo; do
    echo "$f ------------------------------------------------------------"
    echo "$f ------------------------------------------------------------"
    echo "$f ------------------------------------------------------------"
    for seed in 0 1 2 3 4 5 6 7 8 9; do
        cargo run --bin yodel -- --file "$f" --steps 10000 --rng "$seed"
    done
done

