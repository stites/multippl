#!/usr/bin/env sh
cat "$@" | \grep --only-matching '\[.*\]' | sed 's/\[\(.*\)\]/\1/' | awk '
function abs(v) {return v < 0 ? -v : v}
BEGIN {
truth[1] = 0.5;
truth[2] = 1/4;
truth[3] = 3/4;
truth[4] = 7/8;
truth[5] = 1/8;
truth[6] = 7/8;
truth[7] = 6/8;
truth[8] = 5/8;
truth[9] = 4/8;
truth[10] = 0;
truth[11] = 2;
truth[12] = 2;
truth[13] = 0;
}
{
  total=0
  for (i=1;i<=NF;i++) {
    f[i]=abs($i - truth[i]);
    total+=f[i];
  }
  print total;
}'
