#!/usr/bin/env python3
"""Measure the 象/狗/馬 loop's ports from the live turn store.

One entry per `Port` constructor of mathlib4 DarkTower/XiangMaLoopExample.lean
(darktower ac63df59fb), spelled as in the Lean.  The output is read by
DarkTower/XiangMaLoopTraceChecker.lean, which compares each measured grade
with the hand-written table `satietyToday`:

    python3 -I scripts/xiangma_port_export.py \
      | (cd ~/code/mathlib4 && lake env lean --run DarkTower/XiangMaLoopTraceChecker.lean -)

Grading rule (claude-11, packet 3; fixed, not tuned to the data): a port is
"canon" iff eligible > 0 and carrying == eligible, otherwise "payoff".
Eligibility excludes records only for a reason the code gives; each
exclusion cites file:line below and is stated in the port's `measure`.

Unit: one turn record `turn-<id>.json` in the window (by `created_at`).
The window ends `--settle-minutes` (default 35) before now: a turn's later
ports are written up to 30 min after it (`:settle-evidence-delay`,
turn_service.clj:500-504), so a turn still in flight is not yet due to carry
them.  A turn stuck past that margin is counted.
Sidecars (`turn-<id>.json.analysis.json` and the rest, turn_store.clj:50-53)
are not records.  Session-end records (origin "system",
turn_service.clj:640) are not turns, so no port counts them.

The evidence lookups (馬 cards, settled-turn rows) go to Agency's tagged
evidence query; if it cannot be read the exporter exits 2 with no JSON.
`--evidence-file` replaces that query with a JSON list of rows (fixtures).
"""

import argparse
import glob
import json
import os
import re
import sys
import urllib.parse
import urllib.request

SCHEMA = "xiangma.port-measure/v1"
DEFAULT_STORE = os.path.expanduser("~/.emacs-graph/session-turn-analysis")
DEFAULT_MEMORY = os.path.expanduser("~/.claude/projects/-home-joe-code/memory")
DEFAULT_BASE = "http://127.0.0.1:7070"
RECORD_NAME = re.compile(r"^turn-[^.]+\.json$")

# The proforma key, ~/code/CLAUDE.md "Reply proforma" table (lines 345-356).
MARKS = set("㊥🈲㊭🈸㊣㊯🈚㊝㊟㊮🈖🈕🈯🈰㊢🈝㊩🈘㊬㊫🈹🈡🈳")

# 狗's closing line, e.g. "㊢ (m40) used: none · stored: `slug` · queried: ...".
M40_LINE = re.compile(r"^\s*㊢\s*\(m40\)\s*used:", re.M)
STORED = re.compile(r"stored:\s*([^·\n]*)")

# A happened note's commit lines, as turn_acts.clj:96-106 parses them.
COMMIT_SECTION = re.compile(r"Commits during the turn[^\n]*:\n")
COMMIT_LINE = re.compile(r"- (\S+) ([0-9a-f]{7,40}) (.+)")


def load_records(store, since, until=None):
    recs = []
    for path in sorted(glob.glob(os.path.join(store, "turn-*.json"))):
        name = os.path.basename(path)
        if not RECORD_NAME.match(name):
            continue
        try:
            with open(path, encoding="utf-8") as f:
                d = json.load(f)
        except (OSError, ValueError) as e:
            raise SystemExit("xiangma_port_export: unreadable record %s: %s" % (path, e))
        if not isinstance(d, dict) or str(d.get("created_at", "")) < since:
            continue
        if until is not None and str(d.get("created_at", "")) > until:
            continue
        recs.append((name[:-len(".json")], path, d))
    return recs


def fetch_tagged(base, tag, since):
    """Every evidence row with TAG since SINCE, following next-cursor."""
    rows = []
    params = {"tag": tag, "since": since, "limit": "5000"}
    while True:
        url = base.rstrip("/") + "/api/alpha/evidence?" + urllib.parse.urlencode(params)
        req = urllib.request.Request(url, headers={"Accept": "application/json"})
        with urllib.request.urlopen(req, timeout=60) as resp:
            page = json.load(resp)
        if not page.get("ok", True) or "entries" not in page:
            raise ValueError("evidence query for tag %s returned no entries: %r" % (tag, page)[:300])
        rows.extend(page["entries"])
        cursor = page.get("next-cursor")
        if not cursor:
            return rows
        params["cursor-at"] = cursor["at"]
        params["cursor-id"] = cursor["id"]


def evidence_rows(args, since):
    if args.evidence_file:
        with open(args.evidence_file, encoding="utf-8") as f:
            rows = json.load(f)
        source = args.evidence_file
    else:
        try:
            rows = (fetch_tagged(args.base, "ma-observation", since)
                    + fetch_tagged(args.base, "xiang-turn", since))
        except Exception as e:  # noqa: BLE001 -- any failure is "unavailable"
            raise SystemExit("xiangma_port_export: evidence lookup unavailable at %s: %s"
                             % (args.base, e))
        source = args.base + "/api/alpha/evidence?tag=ma-observation|xiang-turn"
    return rows, source


def tags_of(row):
    return {str(t).lstrip(":") for t in (row.get("evidence/tags") or [])}


def paragraphs(text):
    """Blank-line-separated blocks of TEXT, fenced code removed (the proforma:
    code is not marked, ~/code/CLAUDE.md:338)."""
    kept, fenced = [], False
    for line in text.splitlines():
        if line.lstrip().startswith("```"):
            fenced = not fenced
            kept.append("")
            continue
        kept.append("" if fenced else line)
    return [p for p in re.split(r"\n\s*\n", "\n".join(kept)) if p.strip()]


def all_marked(text):
    ps = paragraphs(text)
    return bool(ps) and all(p.lstrip()[:1] in MARKS for p in ps)


def m40_line(text):
    for line in text.splitlines():
        if M40_LINE.match(line):
            return line
    return None


def stored_slugs(line):
    m = STORED.search(line)
    if not m:
        return None
    body = m.group(1).strip()
    if body.lower().startswith("none"):
        return []
    return [s for s in (x.strip().strip("`").strip() for x in body.split(",")) if s]


def happened_commits(summary):
    parts = COMMIT_SECTION.split(str(summary or ""), maxsplit=1)
    if len(parts) < 2:
        return []
    return [m.groups() for m in (COMMIT_LINE.fullmatch(l.strip()) for l in parts[1].splitlines()) if m]


def port(name, measure, eligible, carrying):
    e, c = len(eligible), sum(1 for r in eligible if carrying(r))
    return {"port": name, "measure": measure, "eligible": e, "carrying": c,
            "grade": "canon" if e > 0 and c == e else "payoff"}


def measure(recs, rows, memory_dir):
    obs = {(r.get("evidence/body") or {}).get("turn-evidence-id")
           for r in rows if "ma-observation" in tags_of(r)}
    settled = {r.get("evidence/id") for r in rows if "xiang-turn" in tags_of(r)}

    turns = [(i, p, d) for i, p, d in recs if d.get("origin") != "system"]
    operator = [t for t in turns if (t[2].get("origin") or "operator") == "operator"]
    status = lambda t: t[2].get("analysis_status")
    reply = lambda t: str(t[2].get("reply_text") or "")

    def tagged(t):
        try:
            with open(t[1] + ".analysis.json", encoding="utf-8") as f:
                return bool(json.load(f).get("sentences"))
        except (OSError, ValueError):
            return False

    def stores(t):
        line = m40_line(reply(t))
        slugs = stored_slugs(line) if line else None
        return slugs is not None and all(
            os.path.exists(os.path.join(memory_dir, s + ".md")) for s in slugs)

    unsigned = {"no-trailers", "trailers-unreadable"}

    return [
        port("operatorText",
             "turn records with a non-blank :source_text, of all turn records",
             turns, lambda t: bool(str(t[2].get("source_text") or "").strip())),
        # A bell turn (origin "agent") is recorded "declared" and settles with
        # no 象 reading by design: turn_service.clj:1003-1006, turn_acts.clj:270.
        port("xiangTags",
             "turn records with a .analysis.json sidecar whose :sentences is non-empty; "
             "excludes analysis_status \"declared\" (bell turns get no 象 reading, "
             "turn_service.clj:1003-1006)",
             [t for t in turns if status(t) != "declared"], tagged),
        port("proposedPatterns",
             "turn records with a :proposed_patterns key, of all turn records",
             turns, lambda t: "proposed_patterns" in t[2]),
        port("agentReply",
             "turn records with a non-blank :reply_text, of all turn records",
             turns, lambda t: bool(reply(t).strip())),
        port("commits",
             "turn records with :happened_summary (the commit list after the trailer "
             "filter, turn_service.clj:976), of all turn records",
             turns, lambda t: "happened_summary" in t[2]),
        # 馬 observes operator chat-turns (and Zai/Kimi harness turn-starts),
        # ma_observe.py:414 with turn_frames.py:451; bell records are not
        # operator turns.  ma-enabled.txt is a rollout list, not an exclusion.
        port("maCard",
             "operator-origin turn records whose :evidence_id is the turn-evidence-id "
             "of a ma-observation evidence row; excludes non-operator records "
             "(ma_observe.py:414 observes operator turns); agents absent from "
             "ma-enabled.txt are NOT excluded",
             operator, lambda t: t[2].get("evidence_id") in obs),
        # align! reads only operator turns: ma_reading.clj:246-257.  Its other
        # conditions (a reply, ports computed) are upstream feeds, so a record
        # missing them stays eligible and counts as not carrying.
        port("maAlignment",
             "operator-origin turn records with :alignment; excludes non-operator "
             "records (ma_reading.clj:246-257 aligns operator turns only); records with no reply or uncomputed ports stay eligible",
             operator, lambda t: bool(t[2].get("alignment"))),
        port("patternVerdict",
             "turn records with a non-empty :pattern_verdicts (per proposed pattern: "
             "used / didn't-apply / missed); the field is not built yet",
             turns, lambda t: bool(t[2].get("pattern_verdicts"))),
        port("outcomeJoin",
             "turn records with a non-empty :outcome_join (the next turn's id and its "
             "象 tags as this turn's outcome); the field is not built yet",
             turns, lambda t: bool(t[2].get("outcome_join"))),
        # Turns that settle with no reading (declared, not-requested) are not
        # written as evidence: turn_service.clj:372-374.
        port("memoryWrite",
             "turn records with an e-xiang-turn-<id> row in the xiang-turn evidence "
             "query; excludes analysis_status \"declared\" and \"not-requested\" "
             "(not written by design, turn_service.clj:372-374)",
             [t for t in turns if status(t) not in ("declared", "not-requested")],
             lambda t: ("e-xiang-turn-" + t[0]) in settled),
        port("citesUsedMemory",
             "turn records whose :reply_text has a line \"㊢ (m40) used: ...\", "
             "of all turn records",
             turns, lambda t: m40_line(reply(t)) is not None),
        port("storesFinding",
             "turn records whose :reply_text has the \"㊢ (m40) used:\" line with a "
             "\"stored:\" field, every stored slug existing as <memory-dir>/<slug>.md, "
             "of all turn records",
             turns, stores),
        # The proforma marks replies to the operator; bells to other agents
        # are not marked: ~/code/CLAUDE.md:338.
        port("marksParagraphs",
             "turn records with a non-blank :reply_text in which every blank-line "
             "paragraph (fenced code removed) opens with a proforma mark; excludes "
             "records with no reply and bell records (origin \"agent\": bells are "
             "not marked, ~/code/CLAUDE.md:338)",
             [t for t in operator if reply(t).strip()], lambda t: all_marked(reply(t))),
        port("commitTrailers",
             "turn records with any commit seen (a commit line in :happened_summary, "
             "turn_acts.clj:96, or a :commits_excluded entry); carrying when no "
             "excluded commit has reason no-trailers or trailers-unreadable "
             "(turn_service.clj:331-334)",
             [t for t in turns if happened_commits(t[2].get("happened_summary"))
              or t[2].get("commits_excluded")],
             lambda t: not any(str(e.get("reason", "")).lstrip(":") in unsigned
                               for e in (t[2].get("commits_excluded") or []))),
    ]


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--store", default=DEFAULT_STORE)
    ap.add_argument("--since", default="2026-10-19")
    ap.add_argument("--memory-dir", default=DEFAULT_MEMORY)
    ap.add_argument("--base", default=DEFAULT_BASE)
    ap.add_argument("--evidence-file")
    ap.add_argument("--settle-minutes", type=float, default=35.0)
    args = ap.parse_args(argv)
    import datetime
    until = (datetime.datetime.now(datetime.timezone.utc)
             - datetime.timedelta(minutes=args.settle_minutes)
             ).strftime("%Y-%m-%dT%H:%M:%SZ")
    if not os.path.isdir(args.store):
        raise SystemExit("xiangma_port_export: no store at %s" % args.store)
    recs = load_records(args.store, args.since, until)
    rows, source = evidence_rows(args, args.since)
    latest = max((str(d.get("created_at")) for _, _, d in recs), default=None)
    json.dump({"schema": SCHEMA, "store": os.path.abspath(args.store),
               "evidence": source,
               "window": {"since": args.since, "until": until, "latest": latest},
               "records": len(recs),
               "ports": measure(recs, rows, args.memory_dir)},
              sys.stdout, ensure_ascii=False, indent=1)
    sys.stdout.write("\n")


if __name__ == "__main__":
    main()
