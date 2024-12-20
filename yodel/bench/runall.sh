#!/usr/bin/env bash

for experiment in grids arrival gossip bayesnets; do
    (cd $experiment && bash ./run.sh "$@") || exit 1
done
