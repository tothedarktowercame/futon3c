#!/usr/bin/env python3
"""grain_check.py — is the enacted change at the grain the cascade chose?

  grain_check.py CASCADE.edn ENACTMENT.edn

DERIVE step 3 chooses the grain: what the state one change alters actually
belongs to. It was the one step of nine with no check, and it is the step
instance 4 got wrong. The cascade answered "at the role"; the first
enactment built a lookup keyed by an agent id and returned a provider, and
nothing compared the two until all three wants came back :partial.

The grain of a resolver is WHAT ITS LOOKUP IS KEYED BY, which is visible in
its argument list:

    (defn seat-for [role])      keyed by :role      — the chosen grain
    (defn provider [agent-id])  keyed by :agent-id  — the grain it was built at

So the check is three comparisons, not one:

  1. the cascade declares :grain {:keyed-by ...} on its grain pattern;
  2. the enactment declares the same, with a resolver as evidence;
  3. that resolver EXISTS, with the recorded argument list, in the file at
     the recorded sha256.

Step 3 is what stops this being two agents agreeing with each other. A
declaration that no code answers to is not evidence.

Exit 1 on any mismatch.
"""
import hashlib, json, os, re, subprocess, sys

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def edn(path):
    r = subprocess.run(["bb", "-e",
        '(require (quote [clojure.edn :as edn]) (quote [cheshire.core :as j]))'
        f'(print (j/generate-string (edn/read-string (slurp "{path}"))))'],
        capture_output=True, text=True, cwd=REPO)
    if r.returncode:
        sys.exit(f"grain_check: cannot read {path}: {r.stderr.strip()[:200]}")
    return json.loads(r.stdout)


def arglist_in(path, fn):
    """The argument list of FN as it stands in PATH, or None."""
    try:
        src = open(os.path.join(REPO, path), encoding="utf-8").read()
    except OSError:
        return None, None
    sha = hashlib.sha256(open(os.path.join(REPO, path), "rb").read()).hexdigest()
    m = re.search(r"\(defn-?\s+" + re.escape(fn) + r"\b(.*?)(\[[^\]]*\])",
                  src, re.S)
    return (m.group(2) if m else None), sha


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    casc, enact = edn(sys.argv[1]), edn(sys.argv[2])
    bad = []

    grain_pat = next((p for k, p in (casc.get("patterns") or {}).items()
                      if "choose-the-grain" in k), None)
    if not grain_pat:
        sys.exit(f"{sys.argv[1]}: no grain-choosing pattern — nothing to check")
    planned = (grain_pat.get("grain") or {}).get("keyed-by")
    if not planned:
        bad.append("the cascade's grain pattern declares no :grain {:keyed-by ...}")

    g = enact.get("grain") or {}
    realised = g.get("keyed-by")
    if not realised:
        bad.append("the enactment declares no :grain {:keyed-by ...}")

    if planned and realised and planned != realised:
        bad.append(f"GRAIN MISMATCH: the cascade chose {planned}, "
                   f"the enactment is keyed by {realised}")

    ev = g.get("evidence") or {}
    fn, path = ev.get("fn"), ev.get("path")
    if not (fn and path):
        bad.append("the enactment's grain has no {:fn :path} evidence")
    else:
        short = fn.split("/")[-1]
        actual, sha = arglist_in(path, short)
        if actual is None:
            bad.append(f"no (defn {short} ...) in {path} — the declared grain "
                       f"answers to no code")
        elif ev.get("arglist") and actual != ev["arglist"]:
            bad.append(f"{short} takes {actual}, the enactment recorded "
                       f"{ev['arglist']}")
        elif ev.get("sha256") and sha != ev["sha256"]:
            bad.append(f"{path} has changed since the grain was recorded "
                       f"({sha[:12]}… vs {ev['sha256'][:12]}…)")

    name = os.path.basename(sys.argv[2])
    print(f"{name}: cascade grain {planned}, enacted grain {realised} — "
          + ("OK" if not bad else f"{len(bad)} PROBLEM(S)"))
    for b in bad:
        print("  FAIL " + b)
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()
