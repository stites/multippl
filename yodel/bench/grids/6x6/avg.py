#!/usr/bin/env python3
import argparse
import time
import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import yodel
truth = yodel.truth6

reserved = ["bench.py", "avg.py"]
mainfiles = [f for f in os.listdir('.') if os.path.isfile(f) and not (f in reserved)]
needs_l1 = lambda main: main[-3:] == ".yo" or main[-3:] == ".py"
pad = max([len(m) for m in mainfiles])

def str_stats(ss):
    s = []
    for metric in ['l1', 'ms', ' #']:
        for m in mainfiles:
            if ss[m][' #'] == 0:
                continue
            out = ss[m][metric] if metric == " #" else sum(ss[m][metric]) / ss[m][' #']
            s.append("{} {}: {}".format(m.ljust(pad, " "), metric, str(out)))
            s.append("\n")
        s.append("\n")
    return s


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="average everything in logs/")
    parser.add_argument("--dir", default="logs/", type=str,)
    args = parser.parse_args()

    date = time.strftime("%Y-%m-%d", time.localtime())

    stats = {k: {" #":0,"ms":[], "l1":[]} for k in mainfiles}
    for day in filter(lambda f: os.path.isdir(args.dir + f), os.listdir(args.dir)):
        day = args.dir + "/" + day + "/"
        for hm in filter(lambda f: os.path.isdir(day + f), os.listdir(day)):
            run = day + "/" + hm + "/"
            for log in filter(lambda l: l[-4:] == ".log", os.listdir(run)):
                main = [m for m in mainfiles if log[:len(m)] == m.replace(".", "-")]
                assert len(main) == 1, f"did not find a main file associated with {log}"
                main = main[0]
                with open(run + "/" + log, "r") as f:
                    out = f.readlines()
                    if len(out) == 0:
                        print(f"warning! {run}/{log} is empty!")
                        continue
                last = out[-1].rstrip()
                if last[-2:] != "ms":
                    # print(f"WARNING! expected {log} to have last line ending in 'ms'")
                    continue
                stats[main]['ms'].append(float(last[:-2]))
                stats[main][' #'] += 1
                if needs_l1(main):
                    out = [float(f) for f in out[-2].rstrip().split()]
                    l1 = sum(yodel.compute_l1(out, truth))
                    stats[main]['l1'].append(l1)
                else:
                    stats[main]['l1'].append(0.0)
            ss = str_stats(stats)
            with open(day + f"/{hm}.stats", "w") as f:
                f.writelines(ss)
            print(run)
            print("".join(["    " + s for s in ss]))

else:
    print("please run as main", file=sys.stderr)
    sys.exit(1)
