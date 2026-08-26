#!/usr/bin/env python3
"""fingerprint_audit.py — apply the whitepaper's witness standard to every
recorded memory USE in a campaign.

The standard (retrieval-whitepaper-v3 §3.1, set by a94A09): a USE claim counts
when the committed artifact carries the memory's fingerprint; prose attribution
alone is design signal, never outcome data. Today nothing in the frame machine
checks this -- `:used-ids` is a Student self-report validated only for
membership in the controller-derived surfaced set.

This script does the check offline, per (frame, attempt, memory):

  tokens  the Lean identifiers the memory names (its :key-api, its backticked
          spans, and dotted/underscored names in its body), and how many of
          them occur in the attempt's archived closing source;
  paste   the longest run of consecutive non-trivial memory-body lines that
          appear verbatim in that source.

Both are reported because they are different evidence. A memory whose named
APIs appear in the artifact while its body does not is a fingerprinted USE.
A memory whose body appears verbatim is a paste -- the f29/f30 failure mode
that `codex-scribe-v1` was written to stop -- and should not be counted as
the same kind of result.

BOTH ARE MEASURED AGAINST THE BASE FILE THE STUDENT WAS HANDED, not against
the empty string. An identifier already present in the base problem file is
not evidence that a memory put it there -- the Student would have read it in
the file regardless. Only tokens and lines NOVEL to the attempt's source
count. Without this subtraction the check reports a high hit rate that means
nothing, since memory, base file and certified head all discuss the same
mathematics in the same vocabulary. The base is resolved from the attempt
receipt's own `:base-revision` + `:problem-path` via apm-lean.

Reads only: campaign frame records on disk + the substrate evidence endpoint.

Usage: python3 fingerprint_audit.py [--campaign ID] [--write PATH] [--json]
"""
import argparse, json, os, re, subprocess, sys, urllib.request

SUBSTRATE = os.environ.get("FUTON1B_BASE", "http://127.0.0.1:7073")
CAMPAIGNS = os.environ.get("APM_CAMPAIGNS", os.path.expanduser("~/code/futon3c/data/apm-campaigns"))
APM_LEAN = os.environ.get("APM_LEAN", os.path.expanduser("~/code/apm-lean"))

USED_IDS = re.compile(r":used-ids\s*\[([^\]]*)\]")
ID_STR = re.compile(r'"(e-[^"]+)"')
PROBLEM_ID = re.compile(r':problem-id\s*"([^"]+)"')
BASE_REV = re.compile(r':base-revision\s*"([0-9a-f]{7,})"')
PROBLEM_PATH = re.compile(r':problem-path\s*"([^"]+)"')

# A Lean-ish identifier: letters/digits/underscore/dot/prime, at least one
# underscore or interior dot, no hyphen or slash (those are Clojure/EDN keys).
LEAN_TOKEN = re.compile(r"[A-Za-z][A-Za-z0-9_'.]{5,}")
# English/EDN noise that survives the shape filter.
STOPWORDS = {
    "evidence.body", "evidence.id", "evidence.at", "evidence.type",
    "evidence.author", "evidence.tags", "evidence.subject", "evidence.claim",
    "how_to_apply", "key_api", "capture_kind", "scribe_author", "draft_source",
    "runner_model", "statement_integrity", "all_file_declarations",
}


def get_text(path, timeout=60):
    req = urllib.request.Request(SUBSTRATE + path, headers={"Accept": "application/edn"})
    return urllib.request.urlopen(req, timeout=timeout).read().decode("utf-8", "replace")


def fetch_memory(mid, cache={}):
    if mid not in cache:
        try:
            cache[mid] = get_text("/api/alpha/evidence/" + urllib.parse.quote(mid, safe=""))
        except Exception as e:
            cache[mid] = ""
            print(f"  ! fetch failed {mid}: {e}", file=sys.stderr)
    return cache[mid]


def body_text(raw):
    """The memory body, with EDN string escapes turned back into text."""
    if not raw:
        return ""
    i = raw.find(":evidence/body")
    j = raw.find(":evidence/session-id")
    seg = raw[i:j] if 0 <= i < j else raw
    return seg.replace("\\n", "\n").replace('\\"', '"')


def tokens_of(body):
    """Lean identifiers the memory names, most-specific first."""
    out = set()
    for m in LEAN_TOKEN.finditer(body):
        t = m.group(0).strip(".").strip("'")
        if "-" in t or "/" in t:
            continue
        if t.lower() in STOPWORDS or t in STOPWORDS:
            continue
        # Require a real word-join: an underscore, a dotted qualified name, or
        # a multi-hump CamelCase name. Mathlib is full of single-word type
        # names carrying neither an underscore nor a dot -- NNReal, ENNReal,
        # MeasureTheory -- and dropping them made a memory whose whole content
        # is `open scoped NNReal ENNReal` score zero tokens, so it read
        # `unwitnessed` in every frame that surfaced it no matter what the
        # student did. Prose capitalises one letter (Trigger, Fourier, Lean);
        # a Lean type name carries two, which is the discriminator.
        if "_" not in t and "." not in t:
            caps = sum(1 for c in t if c.isupper())
            if not (caps >= 2 and any(c.islower() for c in t)):
                continue
        # drop pure prose that happens to be dotted (sentence ends)
        if re.fullmatch(r"[a-z]+\.[a-z]+", t):
            continue
        out.add(t)
    return sorted(out, key=lambda s: (-len(s), s))


def base_file(rev, path, cache={}):
    """The problem file as the Student received it, from apm-lean."""
    key = (rev, path)
    if key not in cache:
        try:
            cache[key] = subprocess.run(
                ["git", "show", f"{rev}:{path}"], cwd=APM_LEAN,
                capture_output=True, text=True, timeout=30, check=True).stdout
        except Exception as e:
            cache[key] = None
            print(f"  ! base blob failed {rev}:{path}: {e}", file=sys.stderr)
    return cache[key]


def paste_run(body, novel_lines):
    """Longest run of consecutive body lines appearing verbatim and NOVEL."""
    best = cur = 0
    n_hit = 0
    for line in body.splitlines():
        s = line.strip()
        if len(s) > 12 and s in novel_lines:
            cur += 1
            n_hit += 1
            best = max(best, cur)
        else:
            cur = 0
    return best, n_hit


def attempts(campaign_dir):
    """(frame, attempt-n, problem, used-ids, source-path) for every attempt."""
    out = []
    for frame_dir in sorted(os.listdir(campaign_dir)):
        live = os.path.join(campaign_dir, frame_dir, "live")
        if not os.path.isdir(live):
            continue
        frame = frame_dir.rsplit("-", 1)[-1]
        for n in (1, 2, 3):
            rec = os.path.join(live, f"student-attempt-{n}.edn")
            if not os.path.exists(rec):
                continue
            t = open(rec, encoding="utf-8", errors="replace").read()
            ids = []
            for block in USED_IDS.findall(t):
                for i in ID_STR.findall(block):
                    if i not in ids:
                        ids.append(i)
            pid = PROBLEM_ID.search(t)
            rev = BASE_REV.search(t)
            ppath = PROBLEM_PATH.search(t)
            srcdir = os.path.join(live, f"student-attempt-{n}-source")
            src = None
            if os.path.isdir(srcdir):
                files = [f for f in os.listdir(srcdir) if f.endswith(".lean")]
                if files:
                    src = os.path.join(srcdir, files[0])
            out.append((frame, n, pid.group(1) if pid else "?", ids, src,
                        rev.group(1) if rev else None,
                        ppath.group(1) if ppath else None))
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--campaign", default="jit-all-open-nontopology-v1")
    ap.add_argument("--write")
    ap.add_argument("--json", action="store_true")
    a = ap.parse_args()

    cdir = os.path.join(CAMPAIGNS, a.campaign)
    rows, summary = [], {"attempts": 0, "attempts-with-used": 0, "use-events": 0,
                         "fingerprinted": 0, "paste": 0, "already-in-base": 0,
                         "unwitnessed": 0, "no-source": 0, "no-base": 0,
                         "unfetchable": 0}

    for frame, n, pid, ids, src, rev, ppath in attempts(cdir):
        summary["attempts"] += 1
        if ids:
            summary["attempts-with-used"] += 1
        source = open(src, encoding="utf-8", errors="replace").read() if src else None
        base = base_file(rev, ppath) if (rev and ppath) else None
        novel_lines = set()
        if source is not None and base is not None:
            base_lines = {l.strip() for l in base.splitlines()}
            novel_lines = {l.strip() for l in source.splitlines()
                           if len(l.strip()) > 12 and l.strip() not in base_lines}
        for mid in ids:
            summary["use-events"] += 1
            raw = fetch_memory(mid)
            body = body_text(raw)
            toks = tokens_of(body)
            row = {"frame": frame, "attempt": n, "problem": pid, "memory": mid,
                   "source": os.path.basename(src) if src else None,
                   "tokens-named": len(toks)}
            if not raw:
                row["verdict"] = "unfetchable"
                summary["unfetchable"] += 1
            elif source is None:
                row["verdict"] = "no-source"
                summary["no-source"] += 1
            elif base is None:
                row["verdict"] = "no-base"
                summary["no-base"] += 1
            else:
                hits = [t for t in toks if t in source]
                novel = [t for t in hits if t not in base]
                best, nhit = paste_run(body, novel_lines)
                row.update({"tokens-hit": len(hits), "tokens-novel": len(novel),
                            "novel-hits": novel[:12],
                            "in-base-already": len(hits) - len(novel),
                            "paste-longest-run": best, "paste-lines-hit": nhit})
                if best >= 3:
                    row["verdict"] = "paste"
                    summary["paste"] += 1
                elif novel:
                    row["verdict"] = "fingerprinted"
                    summary["fingerprinted"] += 1
                elif hits:
                    row["verdict"] = "already-in-base"
                    summary["already-in-base"] += 1
                else:
                    row["verdict"] = "unwitnessed"
                    summary["unwitnessed"] += 1
            rows.append(row)

    out = {"campaign": a.campaign, "substrate": SUBSTRATE,
           "standard": "retrieval-whitepaper-v3 3.1 (artifact carries the memory's fingerprint)",
           "summary": summary, "rows": rows}
    if a.write:
        open(a.write, "w").write(json.dumps(out, indent=1))
        print("wrote", a.write, file=sys.stderr)
    if a.json:
        print(json.dumps(out, indent=1))
    else:
        for r in rows:
            print(f"{r['frame']:>4} a{r['attempt']} {r.get('verdict','?'):>15} "
                  f"novel={r.get('tokens-novel','-')}/{r['tokens-named']:<4} "
                  f"inbase={r.get('in-base-already','-'):<3} "
                  f"run={r.get('paste-longest-run','-'):<3} {r['memory'][:52]}")
        print("\n", json.dumps(summary, indent=1))


if __name__ == "__main__":
    import urllib.parse
    main()
