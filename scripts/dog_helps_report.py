#!/usr/bin/env python3
"""dog_helps_report.py — offline report: is 狗 helping? (E-dog-helps §3, packet A)

Read-only over the 象 operator turn records
(~/.emacs-graph/session-turn-analysis/turn-XXXXXX.json). Computes D1, D2, D4,
D5, D7, D8 with intervals, by arm (dog_recall.arm: "shown"/"withheld"; records
without :arm are reported as their own "pre-arm" column, never mixed in) and
by the two strata (hit-stratum: turns with recalled items vs empty-stratum:
turns with none; and own-project: whether the recalled memory's project is the
answering seat's own). D3 is a typed absence until packet D lands
(dog_buried.said-again). Prints the table and writes it dated under
holes/labs/M-repl-circus/.

    python3 scripts/dog_helps_report.py                     # report
    python3 scripts/dog_helps_report.py --hand-check 30     # hand-check sheet

THE ECHO RULE (D2/D5) reuses dog-provider/use-check's exact definition
(futon3c src/futon3c/agency/dog_provider.clj, use-check / content-bigrams /
echo-min-shared): named = the memory's slug or full handle appears verbatim;
echoed = at least echo-min-shared (2) shared distinctive content bigrams,
where a content word is lowercased, keeps [0-9A-Za-z-], drops stopwords and
words of <=3 letters; a bigram is an adjacent content-word pair. Reused in
Python rather than calling the Clojure because this packet forbids JVM loads;
scripts/test_dog_helps_report.py pins the Python copy's agreement with the
Clojure's recorded outcomes on 3 real records.
"""
import argparse
import datetime
import glob
import json
import math
import os
import re
import sys
from collections import defaultdict

ANALYSIS_DIR = os.path.expanduser("~/.emacs-graph/session-turn-analysis")
MEMORY_PROJECTS_DIR = os.path.expanduser("~/.claude/projects")
OUT_DIR = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                       "holes", "labs", "M-repl-circus")
CORRECTION_KINDS = ("disagree", "redirect", "withdraw")
ECHO_MIN_SHARED = 2          # dog_provider.clj echo-min-shared
# The excursion's sample-size targets (§2), for the progress block.
TARGETS = {"D1": 630, "D2": 1030, "D4": 2300}
# The arm's go-live time is recorded in E-dog-helps.md when packet B loads;
# until then every armed record is newer than this sentinel.
ARM_LIVE = "2026-11-11T23:56:03Z"  # packet B loaded (E-dog-helps §Schedule)

STOPWORDS = set(
    "the a an and or of to in is it for on with that this are was be as at by "
    "from have has not but so we you i they them their our your he she his her "
    "its my me us do does did will would could should can may might if then "
    "than when what which who how all any each about into over such no yes up "
    "out just also now here there".split())


def load_records(directory=ANALYSIS_DIR):
    out = []
    pat = re.compile(r"^turn-[A-Za-z0-9]{6}\.json$")
    for f in glob.glob(os.path.join(directory, "turn-*.json")):
        if not pat.match(os.path.basename(f)):
            continue  # skip .analysis/.patterns/.draft/... sidecars
        try:
            d = json.load(open(f))
        except Exception:
            continue
        if not isinstance(d, dict):
            continue
        if d.get("origin", "operator") != "operator":
            continue
        d["_id"] = os.path.basename(f)[:-5]
        d["_file"] = f
        out.append(d)
    out.sort(key=lambda d: d.get("created_at") or "")
    return out


def session_index(records):
    """session_id -> records sorted by created_at (one pass; next-turn join)."""
    idx = defaultdict(list)
    for r in records:
        idx[r.get("session_id")].append(r)
    for v in idx.values():
        v.sort(key=lambda r: r.get("created_at") or "")
    return idx


# --------------------------------------------------------------------------
# The use-check echo rule, reused exactly (see module docstring)
# --------------------------------------------------------------------------

def content_bigrams(s):
    """Adjacent distinctive content-word pairs — dog_provider/content-bigrams."""
    toks = [t.lower() for t in re.split(r"[^0-9A-Za-z\-]+", str(s)) if t]
    ws = [t for t in toks if len(t) > 3 and t not in STOPWORDS]
    return set(zip(ws, ws[1:]))


def used_kind(memory_handle, reply_text, body):
    """('named'|'echoed'|'unused') by use-check's rule; body may be None."""
    reply = str(reply_text or "")
    slug = memory_handle.rsplit("/", 1)[-1]
    if slug and (memory_handle in reply or slug in reply):
        return "named"
    if body is None:
        return "unused"
    if len(content_bigrams(reply) & content_bigrams(body)) >= ECHO_MIN_SHARED:
        return "echoed"
    return "unused"


def memory_body(handle, projects_dir=MEMORY_PROJECTS_DIR):
    if "/" not in str(handle):
        return None
    project, slug = str(handle).split("/", 1)
    path = os.path.join(projects_dir, project, "memory", slug + ".md")
    if os.path.isfile(path):
        try:
            return open(path, encoding="utf-8", errors="replace").read()
        except Exception:
            return None
    return None


# --------------------------------------------------------------------------
# Joins
# --------------------------------------------------------------------------

def arm_of(record):
    dr = record.get("dog_recall") or {}
    return dr.get("arm") or "pre-arm"


def next_operator_turns(records, record, n, idx=None):
    """The next N operator records with the same session_id, by created_at."""
    if idx is None:
        idx = session_index(records)
    seq = idx.get(record.get("session_id"), [])
    at = record.get("created_at") or ""
    later = [r for r in seq if (r.get("created_at") or "") > at]
    return later[:n]


def correction_acts(record):
    """馬 alignment acts of a correction kind on this operator record."""
    al = record.get("alignment") or {}
    return [a for a in (al.get("acts") or [])
            if a.get("kind") in CORRECTION_KINDS]


def has_not_answered(record):
    al = record.get("alignment") or {}
    return any(a.get("verdict") == "not-answered" for a in (al.get("acts") or []))


def own_project(memory_handle, agent_id):
    """Could the answering seat already see this memory? (§2 stratum 2)

    Rule: the memory's project IS the seat (zai-1/zai-1/...), or the seat is a
    claude/kimi/codex seat whose shared home project is -home-joe-code and the
    memory lives there. Stated because packet C will stamp this properly."""
    project = str(memory_handle).split("/", 1)[0]
    if project == agent_id:
        return True
    return (str(agent_id or "").startswith(("claude", "kimi", "codex"))
            and project == "-home-joe-code")


def own_stratum(item, agent_id):
    """§2 stratum 2 for one recalled item. Packet C stamps :in-seat-index at
    recall time (true / false / "no-index" / "n/a" / "unknown"); use it when
    present. Items recalled before the stamp fall back to own_project's
    inferred rule. Returns True, False, or None (not applicable/unknown)."""
    v = item.get("in-seat-index", "absent")
    if v is True or v is False:
        return v
    if v == "no-index":
        return False
    if v in ("n/a", "unknown"):
        return None
    return own_project(item.get("memory"), agent_id)


def prev_operator_turn(record, idx):
    """The operator record before RECORD in its session, or None."""
    seq = idx.get(record.get("session_id"), [])
    at = record.get("created_at") or ""
    earlier = [r for r in seq if (r.get("created_at") or "") < at]
    return earlier[-1] if earlier else None


def said_again_rows(records, idx):
    """D3: each dog_buried.said-again event, split by what 狗 did with the
    matching memory on the previous operator turn of the session: "shown",
    "withheld", "pre-arm" (recalled before the arm), or "not-recalled"."""
    rows = []
    for r in records:
        for ev in (r.get("dog_buried") or {}).get("said-again") or []:
            prev = prev_operator_turn(r, idx)
            mids = set(ev.get("memory-ids") or [])
            what = "not-recalled"
            if prev is not None:
                recalled = {i.get("memory") for i in (prev.get("dog_recall") or {}).get("items") or []}
                if mids & recalled:
                    what = arm_of(prev)
            rows.append({"record": r["_id"], "act": ev.get("act"), "what": what,
                         "shared": len(ev.get("shared") or [])})
    return rows


# --------------------------------------------------------------------------
# Intervals (Wilson score interval on a proportion)
# --------------------------------------------------------------------------

def wilson(k, n, z=1.96):
    if n == 0:
        return (None, None)
    p = k / n
    denom = 1 + z * z / n
    centre = (p + z * z / (2 * n)) / denom
    half = z * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / denom
    return (max(0.0, centre - half), min(1.0, centre + half))


def fmt_pct(x):
    return "–" if x is None else "%.1f%%" % (100 * x)


def prop_line(k, n):
    lo, hi = wilson(k, n)
    return "%d/%d = %s [%s, %s]" % (k, n, fmt_pct(k / n if n else None),
                                    fmt_pct(lo), fmt_pct(hi))


# --------------------------------------------------------------------------
# Metrics
# --------------------------------------------------------------------------

def collect(records):
    """One row per recalled item, one per turn — the shared substrate."""
    items = []   # {arm, own, used(bool via dog_use), restated, misleading}
    turns = []   # {arm, stratum(hit/empty), corrected_next, not_answered}
    idx = session_index(records)
    for r in records:
        dr = r.get("dog_recall") or {}
        recalled = dr.get("items") or []
        arm = arm_of(r)
        nxt = next_operator_turns(records, r, 3, idx)
        nxt1 = nxt[0] if nxt else None
        du = r.get("dog_use") or {}
        use_by_mem = {i.get("memory"): i for i in (du.get("items") or [])}
        # per-item rows
        for it in recalled:
            handle = it.get("memory")
            body = memory_body(handle)
            row = {"arm": arm, "own": own_stratum(it, r.get("agent_id")),
                   "memory": handle, "record": r["_id"]}
            ui = use_by_mem.get(handle)
            if ui is not None:
                row["used"] = bool(ui.get("used"))
                row["kind"] = ui.get("kind")
            else:
                row["used"] = None  # dog_use not yet written
                row["kind"] = None
            # D2: does Joe echo the memory in the next 3 operator turns?
            restated = False
            for ntr in nxt:
                if used_kind(handle, ntr.get("source_text"), body) in ("named", "echoed"):
                    restated = True
                    break
            row["restated"] = restated
            # D5: used on t AND the next turn's correction acts echo the memory
            mis = []
            if row["used"] and nxt1:
                for a in correction_acts(nxt1):
                    if used_kind(handle, a.get("text"), body) in ("named", "echoed"):
                        mis.append((nxt1["_id"], a.get("text", "")[:80]))
            row["misleading"] = mis
            items.append(row)
        # per-turn rows
        turns.append({
            "arm": arm,
            "stratum": "hit" if recalled else "empty",
            "corrected_next": bool(nxt1 and correction_acts(nxt1)),
            "not_answered": has_not_answered(r),
            "query_ms": dr.get("query-ms"),
            "at": r.get("created_at") or "",
            "id": r["_id"],
        })
    return items, turns


def by_arm(rows, key):
    out = defaultdict(lambda: [0, 0])
    for r in rows:
        v = r.get(key)
        if v is None:
            continue
        out[r["arm"]][0] += 1 if v else 0
        out[r["arm"]][1] += 1
    return out


def render(items, turns, records, arm_live=None):
    L = []
    arms = sorted({r["arm"] for r in items + turns} or ["pre-arm"])
    L.append("# 狗 helps? — offline report (%s)" %
             datetime.date.today().isoformat())
    L.append("")
    L.append("Records read: %d operator turns (%s .. %s). Arm live since: %s."
             % (len(turns), turns[0]["at"] if turns else "–",
                turns[-1]["at"] if turns else "–", arm_live or "not yet (packet B)"))
    L.append("")
    L.append("## D1 — use uplift (recalled items; used share shown − withheld)")
    used = by_arm(items, "used")
    for a in arms:
        L.append("- %-9s %s" % (a + ":", prop_line(used[a][0], used[a][1])))
    own = {"own": [r for r in items if r["own"] is True],
           "other": [r for r in items if r["own"] is False],
           "n/a": [r for r in items if r["own"] is None]}
    for name, rows in own.items():
        if not rows:
            continue
        b = by_arm(rows, "used")
        L.append("  - %-14s %s" % (name + "-project:", " | ".join(
            "%s %d/%d" % (a, b[a][0], b[a][1]) for a in arms)))
    L.append("- uplift: – (needs both arms; today: %s)" %
             ("pre-arm only" if arms == ["pre-arm"] else "see below"))
    L.append("")
    L.append("## D2 — restatement after recall (item echoed in Joe's next 3 turns)")
    res = by_arm(items, "restated")
    for a in arms:
        L.append("- %-9s %s" % (a + ":", prop_line(res[a][0], res[a][1])))
    L.append("")
    L.append("## D3 — said-again events (bury-suppressed rulings)")
    buried = [r for r in records if (r.get("dog_buried") or {}).get("items")]
    if not buried:
        L.append("- typed absence: no dog_buried items recorded yet.")
    elif not any("said-again" in (r.get("dog_buried") or {}) for r in records):
        L.append("- typed absence: dog_buried exists (%d records with items) but "
                 "no record carries dog_buried.said-again yet (packet D)." % len(buried))
    else:
        rows = said_again_rows(records, session_index(records))
        L.append("- said-again events recorded: %d (with ≥3 shared bigrams: %d)"
                 % (len(rows), sum(1 for x in rows if x["shared"] >= 3)))
        for what in ("shown", "withheld", "pre-arm", "not-recalled"):
            k = sum(1 for x in rows if x["what"] == what)
            L.append("  - memory %s on the previous turn: %d" % (what, k))
    L.append("")
    L.append("## D4 — next-turn correction (trend only, §2)")
    for a in arms:
        t = [r for r in turns if r["arm"] == a]
        for st in ("hit", "empty"):
            s = [r for r in t if r["stratum"] == st]
            if s:
                L.append("- %-9s %-6s corrected-next %s ; not-answered %s"
                         % (a + ":", st,
                            prop_line(sum(1 for r in s if r["corrected_next"]), len(s)),
                            prop_line(sum(1 for r in s if r["not_answered"]), len(s))))
    L.append("")
    L.append("## D5 — misleading recall (used on t, correction echoing it on t+1)")
    mis = [(r["record"], r["memory"], m) for r in items for m in r["misleading"]]
    if mis:
        for rec, mem, (nxt, text) in mis:
            L.append("- %s %s -> %s: %s" % (rec, mem, nxt, text))
    else:
        L.append("- none this window.")
    L.append("")
    L.append("## D7 — buried chain (buried -> recalled within 14d -> used)")
    buried = [r for r in records if (r.get("dog_buried") or {}).get("items")]
    if not buried:
        L.append("- typed absence: no dog_buried items recorded yet.")
    else:
        L.append("- %d records with buried items; chain joins not yet computed "
                 "(needs 狗-project recalls to accumulate)." % len(buried))
    L.append("")
    L.append("## D8 — cost (dog_recall.query-ms, by ISO week)")
    weeks = defaultdict(list)
    for r in turns:
        if r["query_ms"] is not None:
            weeks[r["at"][:10]].append(r["query_ms"])
    for day in sorted(weeks):
        v = sorted(weeks[day])
        L.append("- %s: n=%d p50=%dms max=%dms" %
                 (day, len(v), v[len(v) // 2], v[-1]))
    if not weeks:
        L.append("- typed absence: no query-ms recorded.")
    L.append("")
    L.append("## Progress toward the §2 targets (since the arm went live)")
    since = arm_live or ""
    live_turns = [t for t in turns if t["at"] >= since] if since else []
    live_items = ([r for r in items if r["arm"] != "pre-arm"] if since else [])
    n_items = len(items) if not since else len(live_items)
    for metric, target in TARGETS.items():
        if not since:
            L.append("- %s: target %s items/turns; arm not live yet — "
                     "pre-arm observations: %s" %
                     (metric, target,
                      len(items) if metric in ("D1", "D2") else len(turns)))
        else:
            cur = (len([r for r in live_items]) if metric in ("D1", "D2")
                   else len(live_turns))
            L.append("- %s: %d / %s (%.0f%%)" % (metric, cur, target,
                                                 100 * cur / target))
    L.append("")
    return "\n".join(L)


def hand_check(records, n, out_path):
    """Sample turns with a correction act; sheet for a reader to mark."""
    cand = [(r, a) for r in records for a in correction_acts(r)]
    step = max(1, len(cand) // n)
    sample = cand[::step][:n]
    L = ["# 狗 hand-check sheet (%s) — mark each: correction-of-agent / change-of-plan"
         % datetime.date.today().isoformat(), ""]
    for i, (r, a) in enumerate(sample, 1):
        paras = [p for p in str(r.get("reply_text") or "").split("\n\n") if p.strip()]
        quote = str(a.get("quote") or "")
        para = next((p for p in paras if quote and quote[:40] in p), paras[0] if paras else "(no reply)")
        L.append("%d. %s — act %s (%s)" % (i, r["_id"], a.get("kind"), a.get("act")))
        L.append("   Joe: %s" % str(a.get("text"))[:200])
        L.append("   Reply para: %s" % para[:300])
        L.append("   Mark: [ ] correction of the agent  [ ] change of plan")
        L.append("")
    open(out_path, "w").write("\n".join(L))
    return len(sample)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--records-dir", default=ANALYSIS_DIR)
    ap.add_argument("--hand-check", type=int, metavar="N", default=None,
                    help="write a sheet of N sampled correction turns instead of the report")
    ap.add_argument("--stdout-only", action="store_true")
    args = ap.parse_args(argv)
    records = load_records(args.records_dir)
    if args.hand_check is not None:
        out = os.path.join(OUT_DIR, "dog-helps-hand-check-%s.md"
                           % datetime.date.today().isoformat())
        n = hand_check(records, args.hand_check, out)
        print("hand-check sheet: %s (%d turns)" % (out, n))
        return 0
    items, turns = collect(records)
    report = render(items, turns, records, arm_live=ARM_LIVE)
    print(report)
    if not args.stdout_only:
        out = os.path.join(OUT_DIR, "dog-helps-%s.md"
                           % datetime.date.today().isoformat())
        with open(out, "w") as fh:
            fh.write(report + "\n")
        print("\n(written: %s)" % out, file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
