#!/usr/bin/env python3
"""Summarize perf-suite variance across repeated runs.

Usage: variance.py <label>=<glob> [<label>=<glob> ...]
Parses the suite's stdout lines and reports, per benchmark and per label,
the SECONDS values across runs: n, min, median, max, mean, and CV%.
Then A/B's the two labels on medians (robust to a single outlier run).
"""
import sys, glob, re, statistics as st

LINE = re.compile(r'^\s{2}(\S+)\s+(.*)$')

def parse(path):
    """benchmark -> seconds, for one run's log."""
    out = {}
    for line in open(path, errors='replace'):
        m = LINE.match(line.rstrip('\n'))
        if not m:
            continue
        name, rest = m.group(1), m.group(2)
        sec = re.search(r'SECONDS\s+([0-9.]+)', rest)
        if sec:
            out[name] = float(sec.group(1))
    return out

def collect(pattern):
    runs = [parse(p) for p in sorted(glob.glob(pattern))]
    return [r for r in runs if r]

def stats(vals):
    n = len(vals)
    mean = st.mean(vals)
    cv = (st.stdev(vals) / mean * 100) if n > 1 and mean else 0.0
    return n, min(vals), st.median(vals), max(vals), mean, cv

def main():
    groups = {}
    order = []
    for arg in sys.argv[1:]:
        label, pattern = arg.split('=', 1)
        groups[label] = collect(pattern)
        order.append(label)

    names = []
    for label in order:
        for run in groups[label]:
            for k in run:
                if k not in names:
                    names.append(k)

    for label in order:
        runs = groups[label]
        print(f"\n=== {label}  ({len(runs)} runs) — SECONDS ===")
        print(f"{'benchmark':<32}{'n':>3} {'min':>9} {'median':>9} {'max':>9} {'CV%':>7}")
        for nm in names:
            vals = [r[nm] for r in runs if nm in r]
            if not vals:
                continue
            n, lo, med, hi, mean, cv = stats(vals)
            print(f"{nm:<32}{n:>3} {lo:>9.3f} {med:>9.3f} {hi:>9.3f} {cv:>6.1f}%")

    if len(order) == 2:
        a, b = order
        print(f"\n=== {b} vs {a} on MEDIAN seconds (>1 means {b} faster) ===")
        print(f"{'benchmark':<32}{a:>11}{b:>11}{'speedup':>10}   note")
        for nm in names:
            va = [r[nm] for r in groups[a] if nm in r]
            vb = [r[nm] for r in groups[b] if nm in r]
            if not va or not vb:
                print(f"{nm:<32}{'-' if not va else f'{st.median(va):.3f}':>11}"
                      f"{'-' if not vb else f'{st.median(vb):.3f}':>11}"
                      f"{'n/a':>10}   benchmark absent from one side")
                continue
            ma_, mb = st.median(va), st.median(vb)
            _, _, _, _, _, cva = stats(va)
            _, _, _, _, _, cvb = stats(vb)
            speed = ma_ / mb if mb else float('nan')
            # Flag when the gap is small relative to observed noise.
            gap = abs(ma_ - mb) / max(ma_, mb) * 100
            note = "WITHIN NOISE" if gap < max(cva, cvb) else ""
            print(f"{nm:<32}{ma_:>11.3f}{mb:>11.3f}{speed:>9.2f}x   {note}")

main()
