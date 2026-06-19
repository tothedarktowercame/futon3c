#!/usr/bin/env python3
"""M-typed-holes micro-dogfood: queries-as-scopes over real mined CT.

Runs the `ScopeQuery` semantics (the Lean spec in mathlib4/DarkTower/ScopeQuery.lean)
as a runtime over an EXISTING local golden graph — the structure-mined CT for
arXiv 0809.2517. A *query is a partial hyperedge*: bound role→node constraints
plus role→variable holes; *answering = unifying against the store* (the graph's
hyperedges), the holes filled by matching nodes. Same machinery that types a
mission (E-mission-head) and a paper (PaperExample) — here, a live query.

This is the runtime side of the Lean spec; it generalises the minimal positional
unifier to set-valued role queries (a role hole returns ALL nodes at that role),
which is what real "which X?" questions want. Stdlib-only, deterministic.
"""
from __future__ import annotations

import re
from pathlib import Path

GRAPH = Path("/home/joe/code/futon6/holes/golden-graphs/"
             "0809.2517-proof-goal-decomposition.edn")

_HEDGE = re.compile(r":kind :([\w-]+),\s*:ends\s*\[(.*?)\]", re.S)
_END = re.compile(r":role :([\w-]+),\s*:node :([\w?-]+)")


def load_store(path: Path):
    """[(kind, [(role, node), …]), …] from a golden-graph EDN's hyperedges."""
    text = path.read_text()
    store = []
    for m in _HEDGE.finditer(text):
        kind = m.group(1)
        ends = [(r, n) for r, n in _END.findall(m.group(2))]
        if ends:
            store.append((kind, ends))
    return store


def answers(store, kind=None, bound=(), holes=()):
    """ScopeQuery.answers, runtime form. A query = a partial hyperedge:
      kind   — required edge kind (or None to match any),
      bound  — [(role, node)] constraints that must be present,
      holes  — [(role, var)] variable ends to fill.
    Returns [binding], one per (edge, hole-match): {var: node}. A role hole is
    set-valued (every node at that role in a matching edge answers)."""
    out = []
    for ekind, ends in store:
        if kind is not None and ekind != kind:
            continue
        if not all(any(er == r and en == n for er, en in ends) for r, n in bound):
            continue
        # each hole: collect every node at that role in this edge
        per_hole = []
        for role, var in holes:
            matches = [(var, en) for er, en in ends if er == role]
            per_hole.append(matches)
        # cartesian over holes (single hole here, but general)
        def expand(idx, acc):
            if idx == len(per_hole):
                out.append(dict(acc)); return
            for binding in per_hole[idx]:
                expand(idx + 1, acc + [binding])
        if per_hole and all(per_hole):
            expand(0, [])
    return out


def _q(label, **kw):
    res = answers(STORE, **kw)
    bound = kw.get("bound", ())
    holes = kw.get("holes", ())
    scope = "{:kind :%s" % kw.get("kind")
    scope += "".join(" (%s :%s)" % (r, n) for r, n in bound)
    scope += "".join(" (%s ?%s)" % (r, v) for r, v in holes) + "}"
    fills = sorted({n for b in res for n in b.values()})
    print(f"{label}\n  scope:  {scope}\n  answer: {fills}\n")


if __name__ == "__main__":
    STORE = load_store(GRAPH)
    print(f"store: {len(STORE)} hyperedges from {GRAPH.name}\n")
    # Q1 (paper/grounding domain): which concept grounds symbol A?
    _q("Q1  which concept grounds symbol A?",
       kind="bind", bound=[("subject", "A")], holes=[("concept", "c")])
    # Q2 (decomposition): what does the goal decompose into?
    _q("Q2  what does the goal decompose into?",
       kind="decomposes-into", bound=[("goal", "goal-statement")],
       holes=[("subgoal", "s")])
    # Q3 (hypothesis): what is the goal's hypothesis?
    _q("Q3  what is the goal's hypothesis?",
       kind="goal", holes=[("hypothesis", "h")])
    # Q4 (open relation, (? knows ?)-style): every bind's subject->concept pair
    _q("Q4  every bind: which subjects bind which concepts?",
       kind="bind", holes=[("subject", "x"), ("concept", "c")])
