#!/usr/bin/env python3
"""giraffe_party.py — the Gauntlet party orderings as a saved script.

M-repl-circus / M-giraffe, 2026-11-12 (kimi-2, requisition via claude-13).
Joe: "it could be interesting to have views of the top ten priorities for
each of our Gauntlet characters and a party view, like we had before."

Rebuilds the 2026-11-10 orderings (M-giraffe.md §"Orderings: Giraffe is the
warrior, not yet the party"; M-peripheral-gauntlet.md "Checkpoint 2"), which
were computed inline in claude-13's session and never saved.

READ-ONLY over every input; the only write is the output party.json.

Sources:
  - Thor:   the newest Giraffe run's pass.edn under ~/storage/giraffe
            (Giraffe's own order: unblocks, then cone).
  - Merlin: convergence scores (unfinished features within two hops),
            recomputed from the lexicon merge
            (~/notes/cascade-live-outline/lexicon/merge/all.json), the same
            measure as ~/notes/claude-13/lexicon-graph/build.py; plus the
            item->named-entities column of the pending index.
  - Questor: pending-index label "ready (one small handoff)".
  - Thyra:  pending-index harm line (H1..H7); readiness = Giraffe pass rank.
  - Titles: the item's holes/ document, via the corpus members.json
            (same resolution as gcc_preview.py).

Output: ~/storage/gcc-preview/party.json in the format of
~/notes/claude-13/party-contract.md. Every reason is words with the number
that drove it. An item with no score is a typed absence, never rank 0.

Usage: python3 scripts/giraffe_party.py [--run DIR] [--pending FILE]
       [--lexicon FILE] [--corpus DIR] [--out DIR] [--top N] [--party N]
"""

import argparse
import heapq
import json
import os
import re
import sys
from datetime import datetime, timezone

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import gcc_preview  # edn_load, read_lines, doc_title, load_corpus  (same dir)
from giraffe_run_data import load_run_file

STORAGE = os.path.expanduser("~/storage")
NOTES = os.path.expanduser("~/notes")

# ------------------------------------------------------------- inputs ----

def newest_run(storage_root=STORAGE):
    base = os.path.join(storage_root, "giraffe")
    runs = sorted(d for d in os.listdir(base)
                  if os.path.isfile(os.path.join(base, d, "pass.edn")))
    if not runs:
        raise SystemExit("no giraffe runs with pass.edn under " + base)
    return os.path.join(base, runs[-1]), runs[-1]


def load_pass(run_dir):
    return load_run_file(os.path.join(run_dir, "pass.edn"), gcc_preview.edn_load)


def load_pending(path):
    with open(path, encoding="utf-8") as f:
        return {row["id"]: row for row in json.load(f)}


def load_lexicon(path):
    with open(path, encoding="utf-8") as f:
        return json.load(f)


# ------------------------------------------------- convergence scores ----

def _aliases(row):
    out = [row.get("entity") or "", row.get("name") or ""]
    out += [a.strip() for a in str(row.get("aka") or "").split(",")]
    return [a for a in out if a]


def convergence_scores(lexicon_rows):
    """Unfinished (partly|hypothesised) features within two hops of each
    entity — the measure of ~/notes/claude-13/lexicon-graph/build.py,
    recomputed from the lexicon merge (adjacency over resolved relations,
    self-links excluded, undirected for neighbourhood purposes)."""
    ents = {}
    for row in lexicon_rows:
        ents.setdefault(row["entity"], row)
    alias = {}
    for row in lexicon_rows:
        for a in _aliases(row):
            alias.setdefault(a.lower(), row["entity"])
    adj = {k: set() for k in ents}
    for row in lexicon_rows:
        for part in str(row.get("relations") or "").split(";"):
            m = re.match(r"\s*([a-z-]+)\s+(.*)", part)
            if not m:
                continue
            for t in re.sub(r"\([^)]*\)", "", m.group(2)).split(","):
                t = alias.get(t.strip().lower())
                if t and t != row["entity"] and t in ents:
                    adj[row["entity"]].add(t)
                    adj[t].add(row["entity"])
    status = {k: (e.get("status") or "") for k, e in ents.items()}
    pend = {k for k in ents if status[k] in ("partly", "hypothesised")}
    return {k: len(((set().union(*(adj[n] for n in adj[k])) if adj[k] else set())
                    | adj[k]) & pend - {k})
            for k in ents}


# ----------------------------------------------------------- the party ----

def item_feature(item_id, slugs):
    """The feature the item IS: strip the M-/E-/T- kind prefix; the rest is
    already the lexicon slug (hyphenated, lowercase)."""
    m = re.match(r"^[MET]-(.+)$", item_id)
    slug = m.group(1) if m else item_id
    return slug if slug in slugs else None


def thor_order(pass_order, top=10):
    """Giraffe's own order (unblocks, then cone), first `top`."""
    out = []
    for entry in pass_order[:top]:
        rank = entry.get("rank")
        # A missing rank is said, never printed as rank 0 (the reason must
        # be true of its basis; futon5 Circus.Characters thor_terms_faithful).
        out.append({"item": entry["id"],
                    "score": entry.get("unblocks"),
                    "reason": "; ".join([
                        "unblocks %d item(s)" % entry["unblocks"]
                        if entry.get("unblocks") is not None else
                        "unblocks 0 item(s) (count missing)",
                        "cone of %d behind it" % entry["cone"]
                        if entry.get("cone") is not None else
                        "cone of 0 behind it (count missing)",
                        "Giraffe pass rank %d" % rank if rank is not None else
                        "no Giraffe pass rank"])})
    return out


def thor_absences(pass_order, top=10):
    """Missing counts in Thor's displayed slice are reading-contract violations.

    Match Circus.Characters.passViolations; measured zero is not missing.
    """
    return [{"what": "%s for %s" % (field, entry["id"]),
             "reason": "Giraffe pass entry has no %s count "
                       "(reading contract violation); shown as 0" % field}
            for entry in pass_order[:top]
            for field in ("unblocks", "cone") if entry.get(field) is None]


def merlin_order(pending, scores, slugs, top=10):
    """Convergence of the feature the item IS, plus half the convergence of
    the strongest unfinished feature it names. "may be no longer wanted"
    items are dropped. An item with no lexicon feature is a typed absence,
    never rank 0."""
    named = {}
    for iid, row in pending.items():
        cands = [scores[e] for e in (row.get("entities") or [])
                 if e in scores and e in slugs_unfinished(slugs)]
        named[iid] = max(cands) if cands else None
    rows, absences = [], []
    for iid, row in pending.items():
        if row.get("pending_label") == "may be no longer wanted":
            continue
        feat = item_feature(iid, slugs["all"])
        own = scores.get(feat) if feat else None
        if own is None:
            absences.append({"what": "merlin score for %s" % iid,
                             "reason": "no lexicon feature named %s"
                                       % (feat or iid)})
            continue
        half = (named[iid] or 0) / 2.0
        rows.append({"item": iid, "score": own + half,
                     "reason": "is %s (convergence %d, unfinished work within "
                               "two hops)%s" %
                               (feat, own,
                                ("; names %s (convergence %d, halved)"
                                 % (best_named(row, scores, slugs), named[iid]))
                                if named[iid] else "")})
    rows.sort(key=lambda r: (-r["score"], r["item"]))
    return rows[:top], absences


def best_named(row, scores, slugs):
    cands = [e for e in (row.get("entities") or [])
             if e in scores and e in slugs_unfinished(slugs)]
    return max(cands, key=lambda e: scores[e]) if cands else None


def slugs_unfinished(slugs):
    return slugs["unfinished"]


def questor_order(pending, scores, slugs, top=10):
    """"ready (one small handoff)" items, strongest feature first. Items
    naming no scored feature rank after scored ones (typed absence, not
    rank 0)."""
    rows, absences = [], []
    for iid, row in pending.items():
        if row.get("pending_label") != "ready (one small handoff)":
            continue
        best = None
        for e in (row.get("entities") or []):
            if e in scores and (best is None or scores[e] > scores[best]):
                best = e
        if best is None:
            absences.append({"what": "questor score for %s" % iid,
                             "reason": "names no lexicon feature with a "
                                       "convergence score"})
        rows.append({"item": iid, "score": scores.get(best),
                     "_named": best,
                     "reason": ("ready (one small handoff); names %s "
                                "(convergence %d)" % (best, scores[best]))
                               if best else
                               "ready (one small handoff); names no scored "
                               "feature"})
    rows.sort(key=lambda r: (-(r["score"] or -1), r["item"]))
    for r in rows:
        r.pop("_named", None)
    return rows[:top], absences


def thyra_order(pending, pass_rank, top=10):
    """Items on a harm line (H1..H7), most ready first (Giraffe pass rank;
    items outside the pass last, by pending level)."""
    rows = []
    for iid, row in pending.items():
        harm = (row.get("harm") or "").strip()
        if not harm:
            continue
        rank = pass_rank.get(iid)
        rows.append({"item": iid, "score": None,
                     "_rank": rank if rank is not None else 10 ** 6,
                     "_level": row.get("pending") or 99,
                     "reason": "on harm line %s; %s" %
                               (harm,
                                ("Giraffe pass rank %d" % rank)
                                if rank is not None else
                                "not in the Giraffe pass; pending level %s"
                                % row.get("pending"))})
    rows.sort(key=lambda r: (r["_rank"], r["_level"], r["item"]))
    for r in rows:
        r.pop("_rank", None)
        r.pop("_level", None)
    return rows[:top], []


def input_needs(inputs):
    """Pinned edges already have needs direction; alternatives do not count."""
    needs = {}
    for edge in inputs["edges"]:
        if edge.get("class") != "alternative":
            needs.setdefault(edge["from"], set()).add(edge["to"])
    return needs


def party_vote(orderings, count=12, skip_seen=True, needs=None, cascade=None):
    """Round robin with complete prerequisite chains before ordinary picks.

    Choose prerequisites by earliest (position, character) in the original
    character lists, then id; unlisted items follow listed ones. A pulled run
    shares its ordinary pick's character and pulled_for. Complete chains that
    exceed capacity consume the turn but are deferred, never partly emitted.
    """
    needs = needs or {}
    cascade = cascade if cascade is not None else {}
    cascade.update(cascade_exceptions=[], cascade_deferred=[], capacity=count)
    ids = list(orderings)
    positions, natural = {}, {}
    for ci, character in enumerate(ids):
        natural[character] = {r["item"]: r["reason"] for r in orderings[character]}
        for pos, row in enumerate(orderings[character]):
            key = (pos, ci, row["item"])
            positions[row["item"]] = min(positions.get(row["item"], key), key)

    def preference(item):
        return positions.get(item, (float("inf"), float("inf"), item))

    chosen, seen, ptr = [], set(), 0
    while len(chosen) < count and any(orderings[c] for c in ids):
        c = ids[ptr % len(ids)]
        ptr += 1
        if not orderings[c]:
            continue
        cand = orderings[c].pop(0)
        while skip_seen and cand["item"] in seen and orderings[c]:
            cand = orderings[c].pop(0)
        if skip_seen and cand["item"] in seen:
            continue
        root = cand["item"]
        chain, indices, item, cycle = [], {}, root, []
        while item not in seen:
            if item in indices:
                cycle = chain[indices[item]:]
                break
            indices[item] = len(chain)
            chain.append(item)
            prerequisites = needs.get(item, set())
            if not prerequisites or prerequisites & seen:
                break
            item = min(prerequisites, key=preference)
        if not chain and not skip_seen:
            chain = [root]
        if cycle:
            cascade["cascade_exceptions"].append({"item": root, "cycle": cycle})
            chain = [root]
        if len(chain) > count - len(chosen):
            cascade["cascade_deferred"].append({"item": root, "needs_slots": len(chain)})
            continue
        for item in reversed(chain):
            row = {"item": item, "chosen_by": c}
            if item == root:
                row["reason"] = cand["reason"]
            else:
                row["pulled_for"] = root
                clause = natural[c].get(item)
                row["reason"] = "needed by " + root + ("; " + clause if clause else "")
            seen.add(item)
            chosen.append(row)
    return chosen


# ---------------------------------------------------------------- main ----

def load_input(run_dir):
    return load_run_file(os.path.join(run_dir, "input.edn"), gcc_preview.edn_load)


def party_dependencies(rows, inputs):
    """Annotate the unchanged party order from its pinned pass input.

    pass.clj:56-61,337 uses from -> to for needs, excluding alternatives
    but including external needs. readings.clj:110-114 already reverses raw
    unblocks into this direction; never reverse the input edges again.
    Completed prerequisites remain disclosed, even when already satisfied.
    """
    ranks = {row["item"]: row["rank"] for row in rows}
    states = {item["id"]: item.get("state-group") for item in inputs["items"]}
    needs = {item: set() for item in ranks}
    for edge in inputs["edges"]:
        if edge["from"] in needs and edge.get("class") != "alternative":
            needs[edge["from"]].add(edge["to"])
    top = [dict(row, needs=[{"item": item, "shown_rank": ranks.get(item),
                             "state": states.get(item)}
                            for item in sorted(needs[row["item"]])]) for row in rows]
    unshown = sorted(set().union(*needs.values()) - ranks.keys()) if needs else []
    waiting = {item: prerequisites & ranks.keys() for item, prerequisites in needs.items()}
    followers = {item: [] for item in ranks}
    for item, prerequisites in waiting.items():
        for prerequisite in prerequisites:
            followers[prerequisite].append(item)
    ready = [(ranks[item], item) for item in ranks if not waiting[item]]
    heapq.heapify(ready)
    linearised = []
    while ready:
        _, item = heapq.heappop(ready)
        linearised.append(item)
        for dependant in followers[item]:
            waiting[dependant].remove(item)
            if not waiting[dependant]:
                heapq.heappush(ready, (ranks[dependant], dependant))
    cycle = []
    if len(linearised) != len(rows):
        # Follow remaining prerequisites until a node repeats: every remaining
        # node has one, so this yields a cycle, excluding any incoming tail.
        item = min((x for x in ranks if waiting[x]), key=ranks.get)
        path, positions = [], {}
        while item not in positions:
            positions[item] = len(path)
            path.append(item)
            item = min(waiting[item], key=ranks.get)
        cycle = path[positions[item]:]
        linearised = None
    return {"top": top, "linearised": linearised, "cycle": cycle,
            "unshown_needs": [{"item": item, "state": states.get(item)} for item in unshown]}


def main(argv=None):
    ap = argparse.ArgumentParser()
    ap.add_argument("--run", default=None, help="giraffe run dir")
    ap.add_argument("--pending",
                    default=os.path.join(NOTES, "cascade-live-outline",
                                         "pending", "pending-index.json"))
    ap.add_argument("--lexicon",
                    default=os.path.join(NOTES, "cascade-live-outline",
                                         "lexicon", "merge", "all.json"))
    ap.add_argument("--corpus",
                    default=os.path.join(NOTES, "wm-next",
                                         "giraffe-reads-2026-11-04", "corpus"))
    ap.add_argument("--out", default=os.path.join(STORAGE, "gcc-preview"))
    ap.add_argument("--top", type=int, default=10)
    ap.add_argument("--party", type=int, default=12)
    args = ap.parse_args(argv)

    run_dir, run_id = (os.path.dirname(args.run), os.path.basename(args.run)) \
        if args.run else newest_run()
    run_dir = args.run or run_dir
    pass_data = load_pass(run_dir)
    pass_order = pass_data["order"]
    pass_rank = {e["id"]: e.get("rank") for e in pass_order}
    pending = load_pending(args.pending)
    # The pending index is a snapshot (2026-11-09); Giraffe's run is current.
    # Drop the items Giraffe's run excluded, so a character never offers
    # an item Giraffe has excluded as finished (claude-13, 2026-11-15: the
    # first live refresh listed M-daily-scan and E-fetch-entity-miss-path,
    # both closed and in the run's excluded.edn).
    excluded_path = os.path.join(run_dir, "excluded.edn")
    excluded = set(re.findall(r':id "([^"]+)"', open(excluded_path, encoding="utf-8").read())) \
        if os.path.isfile(excluded_path) else set()
    pending = {k: v for k, v in pending.items() if k not in excluded}
    lexicon = load_lexicon(args.lexicon)
    scores = convergence_scores(lexicon)
    status = {r["entity"]: (r.get("status") or "") for r in lexicon}
    slugs = {"all": set(status),
             "unfinished": {k for k, s in status.items()
                            if s in ("partly", "hypothesised")}}
    members_list, _, _ = gcc_preview.load_corpus(args.corpus)
    members = {m["id"]: m for m in members_list}

    def title(iid):
        m = members.get(iid)
        p = m.get("path") if m else None
        if p and os.path.isfile(p):
            return gcc_preview.doc_title(gcc_preview.read_lines(p), iid)
        return iid

    thor = thor_order(pass_order, args.top)
    merlin, m_abs = merlin_order(pending, scores, slugs, args.top)
    questor, q_abs = questor_order(pending, scores, slugs, args.top)
    thyra, _ = thyra_order(pending, pass_rank, args.top)

    def dress(rows):
        return [{"rank": i + 1, "item": r["item"], "title": title(r["item"]),
                 "reason": r["reason"], "score": r.get("score")}
                for i, r in enumerate(rows)]

    orderings = {"thor": list(thor), "merlin": list(merlin),
                 "questor": list(questor), "thyra": list(thyra)}
    inputs = load_input(run_dir)
    cascade = {}
    party = party_vote({k: [dict(r) for r in v] for k, v in orderings.items()},
                       args.party, needs=input_needs(inputs), cascade=cascade)
    absences = thor_absences(pass_order, args.top) + m_abs + q_abs
    out = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "sources": {"giraffe_run": run_id,
                    "lexicon": args.lexicon + " (convergence recomputed by "
                               "giraffe_party.py, same measure as "
                             "lexicon-graph/build.py)",
                    "pending_index": args.pending},
        "characters": [
            {"id": "thor", "name": "Thor", "role": "unblocking",
             "rule": "Giraffe's own pass order: what finishing an item "
                     "unblocks, then its cone",
             "top": dress(thor)},
            {"id": "merlin", "name": "Merlin", "role": "strategy",
             "rule": "convergence (unfinished work within two hops) of the "
                     "feature the item is, plus half the strongest unfinished "
                     "feature it names; drops 'may be no longer wanted'",
             "top": dress(merlin)},
            {"id": "questor", "name": "Questor", "role": "quick",
             "rule": "'ready (one small handoff)' items, strongest named "
                     "feature first",
             "top": dress(questor)},
            {"id": "thyra", "name": "Thyra", "role": "harm reduction",
             "rule": "items on a harm line, most ready first (Giraffe pass "
                     "rank)",
             "top": dress(thyra)}],
        "party": {"rule": "round robin Thor, Merlin, Questor, Thyra; skip "
                          "items already chosen; complete prerequisite chains first",
                  "top": [{"rank": i + 1, "item": p["item"],
                           "title": title(p["item"]),
                           "chosen_by": p["chosen_by"], "reason": p["reason"],
                           **({"pulled_for": p["pulled_for"]} if "pulled_for" in p else {})}
                          for i, p in enumerate(party)]},
        "absences": absences}
    out["party"].update(cascade)
    out["party"].update(party_dependencies(out["party"]["top"], inputs))
    os.makedirs(args.out, exist_ok=True)
    path = os.path.join(args.out, "party.json")
    with open(path, "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2, ensure_ascii=False)
        f.write("\n")
    print("wrote", path)
    print("run:", run_id)
    for ch in out["characters"]:
        print(ch["id"], "->", ", ".join(t["item"] for t in ch["top"][:3]))
    print("party:", ", ".join(p["item"] for p in out["party"]["top"]))


if __name__ == "__main__":
    main()
