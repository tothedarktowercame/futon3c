#!/usr/bin/env python3
"""Is the recall receipt-ranking boost load-bearing, or inert?

A READ, not an experiment (Joe's faithful->read / helps->measure rule, 2026-08-02).
The boost (rank-memories, dispatch_with_recall.clj) can only change WHICH memories
surface if, in the same dispatch, BOTH hold:
  (T) truncation binds:  |candidates| > limit (default 5), else `take limit` keeps all
  (V) use-rates vary:    some memory has use-rate>0 so ranking-factor (1.0+alpha*rate,
                         alpha=0.5) is non-uniform; if all factors == 1.0 the ranked
                         order == the pre-receipt order and the surfaced top-k is unchanged.
It is also gated by (S) stats-found? -- at least one candidate carries a prior receipt.

This script checks (S), (T), (V) against the frozen receipts export and reports
whether (T) and (V) ever co-occur. If they never do, the boost is inert in the
corpus and a cohort arm for receipt-ranking would measure nothing.

Reproduce: python3 scripts/receipt_ranking_inertness_check.py
"""
import re
import sys
from collections import Counter
from pathlib import Path

EXPORT = Path("holes/labs/M-memory-retrieval/receipts-export-20260731-all-authors.edn")
LIMIT = 5  # default-limit in dispatch_with_recall.clj

def main() -> int:
    data = EXPORT.read_text()

    # Gate S: how often was the boost even computed?
    s_true = len(re.findall(r":stats-found\?\s+true", data))
    s_false = len(re.findall(r":stats-found\?\s+false", data))

    # Surfaced-set sizes: size==LIMIT means truncation MIGHT have bound.
    surf = (re.findall(r":memory-use/surfaced-ids\s*\[([^\]]*)\]", data)
            + re.findall(r"(?<!use/):surfaced-ids\s*\[([^\]]*)\]", data))
    sizes = [len(re.findall(r'"e-[^"]+"|e-[0-9a-f-]{8,}', s)) for s in surf]

    # For each stats-found? true block, read the per-memory use-rates and count.
    both = []  # dispatches where (T) and (V) co-occur
    blocks = []
    for m in re.finditer(r":stats-found\?\s+true", data):
        w = data[m.start():m.start() + 1500]
        rates = [float(r) for r in re.findall(r":use-rate\s+([0-9.]+)", w)]
        n_cand = len(rates)                    # scored-memory-stats has one entry per candidate
        varies = any(r > 0 for r in rates)     # (V)
        binds = n_cand > LIMIT                 # (T)
        blocks.append((n_cand, max(rates) if rates else 0.0, varies, binds))
        if varies and binds:
            both.append((n_cand, max(rates)))

    print(f"Gate S (stats-found?):  true={s_true}  false={s_false}  "
          f"-> boost not even computed in {s_false}/{s_true+s_false} dispatches")
    print(f"Surfaced-set sizes: {dict(sorted(Counter(sizes).items()))}  "
          f"(size==5 => truncation may bind)")
    print(f"\nstats-found? true blocks: {len(blocks)}")
    for i, (n, mx, v, b) in enumerate(blocks, 1):
        print(f"  block {i:2d}: candidates={n}  max-use-rate={mx:.2f}  "
              f"varies(V)={v}  truncation-binds(T)={b}")
    print(f"\n(T) AND (V) co-occur in: {len(both)} dispatches")
    if not both:
        print("VERDICT: receipt-ranking is INERT across the logged corpus. The boost "
              "is correctly wired (rank before truncate) but the two conditions for it "
              "to change the surfaced set never co-occur: where truncation binds, "
              "use-rates are uniformly 0; where use-rates vary, the candidate set is "
              "below the limit. A cohort arm for receipt-ranking would measure nothing "
              "until the use-attribution channel densifies. Re-run this read to detect "
              "when that changes -- it is not an experiment.")
        return 0
    print("VERDICT: receipt-ranking CAN bind in the corpus; a measurement arm is warranted.")
    return 0

if __name__ == "__main__":
    sys.exit(main())
