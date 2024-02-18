#!/usr/bin/env sh
cat "$@" | \grep --only-matching '\[.*\]' | sed 's/\[\(.*\)\]/\1/' | awk '
{
  for (i=1;i<=NF;i++) {
    f[i]=$i;
  }
  n++
}
END {
  if (n > 0) {
    for (i=1;i<=NF;i++) {
      printf f[i] " "
    };
  print ""
  }
}'
