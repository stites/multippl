#!/usr/bin/env sh
cat "$@" | \grep --only-matching '\[.*\]' | sed 's/\[\(.*\)\]/\1/' | awk '
function abs(v) {return v < 0 ? -v : v}
BEGIN {
truth[1] = 0.5;
truth[2] = 0.29166666666666663;
truth[3] = 0.2743055555555556;
truth[4] = 0.18333333333333335;
truth[5] = 0.10292433261183262;
truth[6] = 0.5993543789019471;
truth[7] = 0.17277777777777778;
truth[8] = 0.1976728617710986;
truth[9] = 0.7709125944790379;
}
{
  total=0
  for (i=1;i<=NF;i++) {
    f[i]=abs($i - truth[i]);
    total+=f[i];
  }
  print total;
}'
