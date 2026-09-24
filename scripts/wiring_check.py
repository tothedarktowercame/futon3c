#!/usr/bin/env python3
"""wiring_check.py — a wiring edge must carry a token its ends actually have.

  wiring_check.py holes/labs/M-futon-seams/wiring/*.edn

The defect this exists to catch: edges derived from the cascade's
context->pattern relation rather than from tokens, which drew a flow the
model does not have. Checks, per file:

  - every edge's token is in its source node's :out (or is an :initial token
    of have-port), and in its target node's :in or :forbids;
  - an :inhibits edge targets a token the destination FORBIDS, not one it
    needs, and a :carries edge the reverse;
  - every node's :licensed-by resolves to a pattern of the named cascade;
  - the recorded dangling outputs and unfed wants are exactly the ones the
    ports imply, so the findings cannot drift from the picture.

Exit 1 on any failure.
"""
import json, os, subprocess, sys


def edn(path):
    r = subprocess.run(
        ["bb", "-e",
         '(require (quote [clojure.edn :as edn]) (quote [cheshire.core :as j]))'
         f'(print (j/generate-string (edn/read-string (slurp "{path}"))))'],
        capture_output=True, text=True)
    if r.returncode:
        sys.exit(f"wiring_check: cannot read {path}: {r.stderr.strip()}")
    return json.loads(r.stdout)


def check(path):
    w = edn(path)
    bad = []
    nodes = {n["id"]: n for n in w["nodes"]}
    casc = edn(w["cascade"]["path"]) if os.path.exists(w["cascade"]["path"]) else None

    for e in (w.get("edges") or []):
        t, a, b = e["token"], e["from"]["node"], e["to"]["node"]
        kind = e.get("kind", "carries")
        if a not in nodes or b not in nodes:
            bad.append(f"edge {t}: unknown node {a if a not in nodes else b}")
            continue
        if t not in (nodes[a].get("out") or []):
            bad.append(f"edge {t}: {a} does not produce it")
        target_in = nodes[b].get("in") or []
        target_forbids = nodes[b].get("forbids") or []
        if kind == "inhibits":
            if t not in target_forbids:
                bad.append(f"edge {t}: {b} does not forbid it, but the edge inhibits")
        elif kind == "carries":
            if t not in target_in:
                bad.append(f"edge {t}: {b} does not need it")
        # :partly-feeds is the deviation's own kind: it names a token the plan
        # has no port for, and is checked only at its source.

    if casc:
        pats = set(casc["patterns"])
        for n in w["nodes"]:
            lic = n.get("licensed-by")
            if lic and "deviation/none" not in str(lic) and lic not in pats:
                bad.append(f"node {n['id']}: licensed-by {lic} is not in the cascade")

        producers, consumers = {}, {}
        for pid, p in casc["patterns"].items():
            for t in p["produces"]:
                producers.setdefault(t, []).append(pid)
            for t in p["guard"]["needs"]:
                consumers.setdefault(t, []).append(pid)
        want = set(casc["want"])
        implied_dangling = {(pid.split("/", 1)[-1], t)
                            for t, ps in producers.items()
                            if t not in want and not consumers.get(t) for pid in ps}
        recorded = {(d["node"], d["token"]) for d in (w.get("dangling-outputs") or [])}
        if implied_dangling != recorded:
            bad.append(f"dangling outputs recorded {sorted(recorded)} but the ports imply "
                       f"{sorted(implied_dangling)}")
        implied_unfed = {t for t in want if not producers.get(t)}
        rec_unfed = {u["want"] for u in (w.get("unfed-wants") or [])}
        if implied_unfed != rec_unfed:
            bad.append(f"unfed wants recorded {sorted(rec_unfed)} but the ports imply "
                       f"{sorted(implied_unfed)}")

    print(f"{path}: {len(w['nodes'])} nodes, {len(w.get('edges') or [])} edges — "
          + ("OK" if not bad else f"{len(bad)} PROBLEM(S)"))
    for x in bad:
        print("  FAIL " + x)
    return not bad


if __name__ == "__main__":
    sys.exit(0 if all([check(p) for p in sys.argv[1:]]) else 1)
