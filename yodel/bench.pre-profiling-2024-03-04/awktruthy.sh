#!/usr/bin/env sh
# example: ../../target/release/yodel --file grids/3x3/grids3x3.yo --steps 1 | ./awktruthy.sh
cat "$@" | \grep --only-matching '\[.*\]' | sed 's/\[\(.*\)\]/\1/' |  \grep --only-matching '[.0-9]*' | awk '{
  print "truth[" NR "] =", $1 ";"
}'
