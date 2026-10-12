#!/usr/bin/env python3
"""Deterministically sample settled operator turns for move labelling."""
import argparse
import json
import random
import re
from pathlib import Path

ROOT = Path.home() / ".emacs-graph/session-turn-analysis"
LABELLED_SESSION = "ec3a9b31-6656-4530-90db-53a22762c17f"
LABELLED_COUNT = 29
OPENERS = {"propose", "ask-action", "offer", "delegate", "promise",
           "report-problem", "clarify", "verify"}
MARK = re.compile(r"^\s*([^\w\s])\s*(?:\((.*?)\)|\[(.*?)\])?\s*(.*)$", re.S)


def load_json(path):
    with path.open(encoding="utf-8") as handle:
        return json.load(handle)


def act_base(record):
    """The act-id base the kernel used for this record: <turn_id>, or
    <turn_id>@<record-id> when the turn id was reused in the session (read
    from the record's stored :acts when present)."""
    turn_id = record.get("turn_id") or ""
    for act in record.get("acts") or []:
        aid = act.get("id") or ""
        for sep in ("-f-", "-r-", "-c-", "-b-"):
            if sep in aid:
                base = aid.split(sep, 1)[0]
                if base.startswith(turn_id):
                    return base
    return turn_id


def fragments(record, record_path):
    analysis_path = Path(record.get("analysis_file") or
                         (str(record_path) + ".analysis.json"))
    if not analysis_path.exists():
        return []
    analysis = load_json(analysis_path)
    out = []
    base = act_base(record)
    for sentence in analysis.get("sentences", []):
        sid = sentence.get("id", "s0")
        for index, fragment in enumerate(sentence.get("fragments", [])):
            item = {"id": f"{base}-f-{sid}-{index}",
                    "intent": fragment.get("intent"),
                    "text": fragment.get("text", "")}
            out.append(item)
    return out


MARKS = set("㊥㊭㊣🈚㊟🈖🈯㊢㊩㊬🈹🈳🈲🈸㊯㊝㊮🈕🈰🈝🈘㊫🈡")


def reply_marks(record):
    """Reply-paragraph acts as the kernel numbers them (turn_acts/reply-acts):
    paragraphs are runs between blank lines; a paragraph is marked when its
    first character is a proforma mark; ids are <turn_id>-r-<i> with i over
    the MARKED paragraphs only, 0-based; text is the body after the mark and
    an optional colon."""
    reply = record.get("reply_text") or ""
    result = []
    index = 0
    for paragraph in re.split(r"\n\s*\n", reply):
        paragraph = paragraph.strip()
        if not paragraph or paragraph[0] not in MARKS:
            continue
        mark = paragraph[0]
        body = re.sub(r"^:\s*", "", paragraph[1:].lstrip())
        result.append({"id": f"{act_base(record)}-r-{index}",
                       "kind": "reply-mark",
                       "author": record.get("agent_id") or "agent",
                       "mark": mark,
                       "text": body})
        index += 1
    return result


OPEN_PORTS_CAP = 24
IN_FORCE_CAP = 16


def act_index(rows):
    """act id -> {kind, text} over every record's stored acts, for resolving
    ids that standings carry without text."""
    index = {}
    for _, record, _ in rows:
        for act in record.get("acts") or []:
            if act.get("id"):
                index.setdefault(act["id"], {"kind": act.get("kind"), "text": act.get("text") or ""})
    return index


def in_force(record, index, cap=IN_FORCE_CAP):
    """The kernel's standings in force at this record (standings.in_force:
    constraints, rules, plans that still govern), resolved to text through
    INDEX, in the record's order, newest last; unresolved ids are listed
    with empty text so the labeller can still cite them."""
    ids = (record.get("standings") or {}).get("in_force") or []
    rows = []
    for aid in ids:
        found = index.get(aid, {})
        rows.append({"act": aid, "kind": found.get("kind"),
                     "text": (found.get("text") or "")[:240]})
    return rows[-cap:]


def open_ports(record, cap=OPEN_PORTS_CAP):
    """The kernel's still-open ports at this record (ports.still_open), oldest
    first, as candidate answer and scope targets from earlier turns of the
    session: act id, kind, text, since.  Capped so an input stays small."""
    ports = (record.get("ports") or {}).get("still_open") or []
    rows = []
    for port in ports:
        if not isinstance(port, dict) or not port.get("act"):
            continue
        rows.append({"act": port["act"], "kind": port.get("kind"),
                     "text": (port.get("text") or "")[:240],
                     "since": port.get("since")})
    rows.sort(key=lambda row: row.get("since") or "")
    return rows[:cap]


def records(root=ROOT):
    loaded = []
    for path in sorted(root.glob("turn-*.json")):
        # glob also sees derived .analysis/.draft files.
        if path.name.count(".json") != 1:
            continue
        try:
            record = load_json(path)
        except (OSError, ValueError):
            continue
        fs = fragments(record, path)
        if (record.get("origin") == "operator"
                and record.get("analysis_status") == "analyzed"
                and (record.get("reply_text") or "").strip()
                and fs):
            loaded.append((path, record, fs))
    return loaded


def choose(rows, n, seed, include_labelled=False):
    rng = random.Random(seed)
    mandatory = [row for row in rows if row[1].get("session_id") == LABELLED_SESSION]
    mandatory.sort(key=lambda row: (row[1].get("created_at", ""), row[0].name))
    mandatory = mandatory[:LABELLED_COUNT]
    target = max(n, len(mandatory)) if include_labelled else n
    chosen = list(mandatory) if include_labelled else []
    chosen_paths = {row[0] for row in chosen}
    counts = {}
    for _, record, _ in chosen:
        session = record.get("session_id")
        counts[session] = counts.get(session, 0) + 1

    def add_from(candidates, wanted):
        rng.shuffle(candidates)
        added = 0
        for row in candidates:
            if len(chosen) >= target or added >= wanted:
                break
            session = row[1].get("session_id") or ""
            if row[0] in chosen_paths or counts.get(session, 0) >= 3:
                continue
            chosen.append(row)
            chosen_paths.add(row[0])
            counts[session] = counts.get(session, 0) + 1
            added += 1

    remaining = max(0, target - len(chosen))
    multi = [row for row in rows if sum(f.get("intent") in OPENERS for f in row[2]) >= 2]
    single = [row for row in rows if sum(f.get("intent") in OPENERS for f in row[2]) == 1]
    add_from(multi, (remaining + 1) // 2)
    add_from(single, remaining // 2)
    # Fill if one stratum cannot meet its half without violating session spread.
    add_from([row for row in rows if row not in chosen], target - len(chosen))
    if len(chosen) != target:
        raise RuntimeError(f"could select only {len(chosen)} of {target} records")
    return sorted(chosen, key=lambda row: row[0].stem)


def write_inputs(chosen, out):
    out.mkdir(parents=True, exist_ok=True)
    by_session = {}
    for row in records(ROOT):
        by_session.setdefault(row[1].get("session_id"), []).append(row)
    for session_rows in by_session.values():
        session_rows.sort(key=lambda row: (row[1].get("created_at", ""), row[0].name))
    written = []
    index = act_index(row for rows in by_session.values() for row in rows)
    for path, record, fs in chosen:
        session_rows = by_session.get(record.get("session_id"), [])
        position = next((i for i, row in enumerate(session_rows) if row[0] == path), -1)
        previous = reply_marks(session_rows[position - 1][1]) if position > 0 else []
        record_id = path.stem
        payload = {"record": record_id, "turn_id": record.get("turn_id"),
                   "session_id": record.get("session_id"),
                   "created_at": record.get("created_at"),
                   "agent_id": record.get("agent_id"),
                   "source_text": record.get("source_text", ""),
                   "fragments": fs, "reply_text": record.get("reply_text"),
                   "reply_acts": reply_marks(record),
                   "previous_reply_acts": previous}
        if record.get("acts") is not None:
            payload["acts"] = record["acts"]
        if record.get("mission_id"):
            payload["mission_id"] = record["mission_id"]
        ports = open_ports(record)
        if ports:
            payload["open_ports"] = ports
        standings = in_force(record, index)
        if standings:
            payload["in_force"] = standings
        target = out / f"{record_id}.input.json"
        target.write_text(json.dumps(payload, ensure_ascii=False, indent=2) + "\n",
                          encoding="utf-8")
        written.append(record_id)
    return written


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--n", type=int, default=0)
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--seed", type=int, default=1)
    parser.add_argument("--include-labelled", action="store_true")
    parser.add_argument("--records", type=Path,
                        help="file of record ids, one per line: select exactly these (ignores --n/--seed)")
    args = parser.parse_args()
    if args.records:
        wanted = {line.strip() for line in args.records.read_text(encoding="utf-8").splitlines() if line.strip()}
        selected = sorted((row for row in records() if row[0].stem in wanted), key=lambda row: row[0].stem)
        missing = wanted - {row[0].stem for row in selected}
        if missing:
            raise SystemExit(f"not eligible or not found: {sorted(missing)}")
    else:
        selected = choose(records(), args.n, args.seed, args.include_labelled)
    for record_id in write_inputs(selected, args.out):
        print(record_id)


if __name__ == "__main__":
    main()
