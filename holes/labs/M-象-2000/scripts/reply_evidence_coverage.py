#!/usr/bin/env python3
"""Read-only coverage census joining xiang records to agent-chat evidence."""

import argparse
import datetime as dt
import hashlib
import json
import pathlib
import sys
import time
import urllib.parse
import urllib.error
import urllib.request
from collections import Counter, defaultdict


RECORD_DIR = pathlib.Path.home() / ".emacs-graph/session-turn-analysis"
BASE = "http://127.0.0.1:7073/api/alpha/evidence"
START = "2026-10-01T00:00:00Z"


def get_json(url, timeout=30, attempts=5):
    request = urllib.request.Request(url, headers={"Accept": "application/json"})
    for attempt in range(attempts):
        try:
            with urllib.request.urlopen(request, timeout=timeout) as response:
                return json.load(response)
        except (urllib.error.HTTPError, urllib.error.URLError, TimeoutError):
            if attempt + 1 == attempts:
                raise
            time.sleep(1.5 * (attempt + 1))


def evidence_pages():
    params = {"tags": "chat,turn", "since": START, "limit": "1000"}
    entries = []
    while True:
        page = get_json(BASE + "?" + urllib.parse.urlencode(params), timeout=60)
        entries.extend(page.get("entries", []))
        cursor = page.get("next-cursor")
        if not cursor:
            return entries
        params["cursor-at"] = cursor["at"]
        params["cursor-id"] = cursor["id"]


def record_files():
    return sorted(p for p in RECORD_DIR.glob("turn-*.json") if p.name.count(".") == 1)


def load_records():
    recent, early = [], []
    for path in record_files():
        try:
            record = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            continue
        created = str(record.get("created_at") or "")
        if record.get("analysis_status") != "analyzed":
            continue
        if (record.get("origin") or "operator") != "operator":
            continue
        item = {"id": path.stem, "record": record}
        if "2026-10-09" <= created[:10] <= "2026-10-11":
            recent.append(item)
        elif "2026-10-01" <= created[:10] <= "2026-10-03":
            early.append(item)
    # Reproducible sample independent of directory order.
    early.sort(key=lambda x: hashlib.sha256(x["id"].encode()).hexdigest())
    chosen = recent + early[:100]
    return sorted(chosen, key=lambda x: (x["record"].get("created_at", ""), x["id"])), len(recent), min(100, len(early))


def body(entry):
    return entry.get("evidence/body") or {}


def eid(entry):
    return entry.get("evidence/id")


def at(entry):
    return str(entry.get("evidence/at") or "")


def is_assistant(entry):
    return body(entry).get("role") == "assistant" and body(entry).get("event") == "chat-turn"


def reply_key(entry):
    return body(entry).get("unified-turn-id") or eid(entry)


def contains_in_order(whole, parts):
    position = 0
    for part in parts:
        found = whole.find(part, position)
        if found < 0:
            return False
        position = found + len(part)
    return True


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--json", action="store_true", help="emit the full machine-readable result")
    args = parser.parse_args()
    read_at = dt.datetime.now(dt.timezone.utc).isoformat().replace("+00:00", "Z")
    records, recent_count, sample_count = load_records()
    entries = evidence_pages()
    by_id = {eid(e): e for e in entries if eid(e)}
    children = defaultdict(list)
    sessions = defaultdict(list)
    assistants = []
    for entry in entries:
        parent = entry.get("evidence/in-reply-to")
        if parent:
            children[parent].append(entry)
        session = entry.get("evidence/session-id")
        if session:
            sessions[str(session)].append(entry)
        if is_assistant(entry):
            assistants.append(entry)
    for values in children.values():
        values.sort(key=lambda e: (at(e), eid(e) or ""))
    for values in sessions.values():
        values.sort(key=lambda e: (at(e), eid(e) or ""))

    # The time window closes at the next selected or unselected operator record
    # in the same session, so load that boundary from every record file.
    operator_times = defaultdict(list)
    for path in record_files():
        try:
            r = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            continue
        if (r.get("origin") or "operator") == "operator" and r.get("session_id") and r.get("created_at"):
            operator_times[str(r["session_id"])].append(str(r["created_at"]))
    for values in operator_times.values():
        values.sort()

    rows = []
    disagreements = []
    for item in records:
        r = item["record"]
        evidence_id = r.get("evidence_id")
        chain_candidates = []
        if evidence_id:
            chain_candidates = [e for e in children.get(str(evidence_id), []) if is_assistant(e)]
            if not chain_candidates:
                for intermediate in children.get(str(evidence_id), []):
                    chain_candidates.extend(e for e in children.get(eid(intermediate), []) if is_assistant(e))
        # De-duplicate a candidate reachable through more than one accidental edge.
        chain_candidates = list({eid(e): e for e in chain_candidates}.values())

        session = str(r.get("session_id") or "")
        created = str(r.get("created_at") or "")
        later = [x for x in operator_times.get(session, []) if x > created]
        boundary = later[0] if later else "9999"
        time_candidates = [e for e in sessions.get(session, [])
                           if is_assistant(e) and at(e) > created and at(e) < boundary]
        time_candidates.sort(key=lambda e: (at(e), eid(e) or ""))
        time_choice = time_candidates[0] if time_candidates else None
        chain_choice = chain_candidates[0] if len(chain_candidates) == 1 else None

        if chain_choice is not None:
            outcome = "join-1"
            choice = chain_choice
        elif len(chain_candidates) > 1:
            outcome = "ambiguous-join-1"
            choice = None
        elif time_choice is not None:
            outcome = "join-2"
            choice = time_choice
        else:
            outcome = "none"
            choice = None
        if chain_choice and time_choice and reply_key(chain_choice) != reply_key(time_choice):
            disagreements.append({"record_id": item["id"], "chain": eid(chain_choice), "time": eid(time_choice),
                                  "chain_key": reply_key(chain_choice), "time_key": reply_key(time_choice)})
        cohort = "recent-all" if created[:10] >= "2026-10-09" else "early-sample"
        rows.append({"record_id": item["id"], "turn_id": r.get("turn_id"), "created_at": created,
                     "cohort": cohort,
                     "session_id": session, "record_evidence_id": evidence_id, "outcome": outcome,
                     "reply_evidence_id": eid(choice) if choice else None,
                     "chain_candidates": [eid(e) for e in chain_candidates],
                     "time_candidate": eid(time_choice) if time_choice else None})

    split_entries = [e for e in assistants if isinstance(body(e).get("segment-index"), int)
                     and body(e)["segment-index"] > 0]
    segment_groups = defaultdict(list)
    for entry in assistants:
        segment_groups[reply_key(entry)].append(entry)
    split_groups = []
    for key, group in segment_groups.items():
        if not any((body(e).get("segment-index") or 0) > 0 for e in group):
            continue
        indexed = sorted((e for e in group if isinstance(body(e).get("segment-index"), int)),
                         key=lambda e: body(e)["segment-index"])
        finals = [e for e in indexed if body(e).get("segment-final") is True]
        concatenated = "".join(str(body(e).get("text") or "") for e in indexed)
        complete_indices = [body(e)["segment-index"] for e in indexed] == list(range(len(indexed)))
        exact = len(finals) == 1 and complete_indices and body(finals[0]).get("unified-text") == concatenated
        covers = (len(finals) == 1 and complete_indices and
                  contains_in_order(str(body(finals[0]).get("unified-text") or ""),
                                    [str(body(e).get("text") or "") for e in indexed]))
        split_groups.append({"key": key, "segments": len(indexed), "finals": len(finals),
                             "complete_indices": complete_indices, "unified_exact": exact,
                             "unified_covers_segments": covers})

    counts = Counter(row["outcome"] for row in rows)
    matched_entries = [by_id.get(row["reply_evidence_id"]) for row in rows if row["reply_evidence_id"]]
    matched_entries = [entry for entry in matched_entries if entry]
    examples = {}
    for wanted in ("join-1", "join-2", "none"):
        examples[wanted] = next((row for row in rows if row["outcome"] == wanted), None)
    result = {
        "read_at": read_at,
        "population": {"recent_all": recent_count, "early_sample": sample_count, "total": len(rows)},
        "evidence_entries_read": len(entries),
        "records_with_evidence_id": sum(bool(row["record_evidence_id"]) for row in rows),
        "outcomes": dict(sorted(counts.items())),
        "split_assistant_entries_index_gt_0": len(split_entries),
        "split_reply_groups": len(split_groups),
        "matched_records_whose_reply_group_is_split": sum(
            len(segment_groups[reply_key(entry)]) > 1 for entry in matched_entries),
        "split_groups_unified_exact": sum(g["unified_exact"] for g in split_groups),
        "split_groups_unified_covers_segments": sum(g["unified_covers_segments"] for g in split_groups),
        "split_groups_not_exact_or_incomplete": sum(not g["unified_exact"] for g in split_groups),
        "split_group_shapes": dict(sorted(Counter(
            "complete-one-final" if g["complete_indices"] and g["finals"] == 1
            else "incomplete-or-multiple-final"
            for g in split_groups).items())),
        "outcomes_by_cohort": {
            cohort: dict(sorted(Counter(row["outcome"] for row in rows if row["cohort"] == cohort).items()))
            for cohort in ("recent-all", "early-sample")
        },
        "join_disagreements": disagreements,
        "examples": examples,
        "rows": rows,
    }
    if args.json:
        json.dump(result, sys.stdout, ensure_ascii=False, indent=2)
        print()
    else:
        print(json.dumps({k: v for k, v in result.items() if k != "rows"}, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
