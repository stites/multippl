#!/usr/bin/env sh

#!/usr/bin/env sh
cat "$@" | \grep --only-matching '\[.*\]' | sed 's/\[\(.*\)\]/\1/' | awk '
function abs(v) {return v < 0 ? -v : v}
BEGIN {
# # gmm_truth
# # ===============================
# truth[1] = 0.3; # hi
# truth[2] = 1.0; # left_hi
# truth[3] = 3.0; # right_hi
# truth[4] = -1.0; # left_lo
# truth[5] = -3.0; # right_lo
#
# # hi_truth
# # ===============================
# truth[6] = 0.3; # x00
# truth[7] = 0.4; # x01t
# truth[8] = 0.6; # x01f
# truth[9] = 0.6; # x10t
# truth[10] = 0.5; # x10f
# truth[11] = 0.6; # x11tt
# truth[12] = 0.7; # x11ft
# truth[13] = 0.8; # x11tf
# truth[14] = 0.9; # x11ff
truth[1] = 0.3; # x00
truth[2] = 0.4; # x01t
truth[3] = 0.6; # x01f
truth[4] = 0.6; # x10t
truth[5] = 0.5; # x10f
truth[6] = 0.6; # x11tt
truth[7] = 0.7; # x11ft
truth[8] = 0.8; # x11tf
truth[9] = 0.9; # x11ff

# # lo_truth
# # ===============================
# truth[15] = 0.2; # x00
# truth[16] = 0.3; # x01t
# truth[17] = 0.4; # x01f
# truth[18] = 0.5; # x10t
# truth[19] = 0.6; # x10f
# truth[20] = 0.1; # x11tt
# truth[21] = 0.2; # x11ft
# truth[22] = 0.3; # x11tf
# truth[23] = 0.4; # x11ff
truth[10] = 0.2; # x00
truth[11] = 0.3; # x01t
truth[12] = 0.4; # x01f
truth[13] = 0.5; # x10t
truth[14] = 0.6; # x10f
truth[15] = 0.1; # x11tt
truth[16] = 0.2; # x11ft
truth[17] = 0.3; # x11tf
truth[18] = 0.4; # x11ff
}
{
  total=0
  for (i=1;i<=NF;i++) {
    f[i]=abs($i - truth[i]);
    total+=f[i];
  }
  print total;
}'
