#!/usr/bin/env bash
# measures in (1000 * 1000): milliseconds
ts=$(date +%s%N) ; "$@" ; tt=$((($(date +%s%N) - ts) / (1000 * 1000) )) ; echo "Time taken: $tt ms"
