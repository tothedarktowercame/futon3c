#!/usr/bin/env python3
"""The single runtime `fill` operator for M-typed-holes (DERIVE D1, invariants
I1/I2/I3/I5).

This is the keystone the six projections route through. One operator,
`fill(hole, filler, kind=...)`, performs the *shared discharge*: it records a
witnessed `(ask -> answer)` pair in ArSE (the I5 witness) and returns a
`FillRecord`.  Each projection is an ADAPTER that maps its own
`(hole, filler)` into one call here -- it does NOT reimplement discharge (I3).

The canonical reference is the gate-b proving loop: posing a scope-query and
recording its answer in ArSE is exactly `fill` for the answer/query projection.
This module generalises that to all six.

Discharge kinds mirror `mathlib4/DarkTower/Coverage.lean`'s `Projection`
(6 constructors).  Stdlib only; the only side effect is the ArSE HTTP write.
"""
from __future__ import annotations

import json
import urllib.error
import urllib.request
from dataclasses import dataclass, field
from typing import Any, Sequence


ARSE_BASE = "http://localhost:7070/api/alpha/arse"
DEFAULT_TAGS = ("m-typed-holes",)

# The six discharge kinds (Coverage.lean Projection). Each projection adapter
# passes its own kind so the single witnessed fill is auditable per-projection.
KIND_CASCADE_FEED = "cascade-feed"   # mining: a hungry node fed by the cascade
KIND_DISCHARGE = "discharge"         # proofs: a sorry discharged by a proof
KIND_GROUND = "ground"               # grounding: an ungrounded symbol -> concept
KIND_COMPOSE = "compose"             # combs: a :composes hole -> sub-process
KIND_ANSWER = "answer"               # queries: a scope variable -> store node
KIND_REPLY = "reply"                 # bells: a type=query -> type=answer

KINDS = frozenset(
    {KIND_CASCADE_FEED, KIND_DISCHARGE, KIND_GROUND, KIND_COMPOSE, KIND_ANSWER, KIND_REPLY}
)


@dataclass(frozen=True)
class TypedHole:
    """A position awaiting a filler. `hungry_for` is the hole's type (I1)."""
    id: str
    hungry_for: str
    projection: str                       # one of KINDS (the source surface)
    source: dict[str, Any] = field(default_factory=dict)


@dataclass(frozen=True)
class FillRecord:
    """The result of a discharge: the ArSE witness IDs (I5) for the fill."""
    hole_id: str
    kind: str
    filled: bool
    filler: Any
    thread_id: str | None
    question_evidence: str | None
    answer_evidence: str | None
    open_note: str | None = None


def _http_json(method: str, path: str, payload: dict[str, Any] | None = None) -> dict[str, Any]:
    data = json.dumps(payload).encode("utf-8") if payload is not None else None
    headers = {"Content-Type": "application/json"} if data else {}
    req = urllib.request.Request(f"{ARSE_BASE}{path}", data=data, headers=headers, method=method)
    try:
        with urllib.request.urlopen(req, timeout=20) as res:
            return json.loads(res.read().decode("utf-8"))
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise RuntimeError(f"{method} {path} failed: HTTP {exc.code}: {detail}") from exc


def type_matches(hole: TypedHole, filler_type: str | None) -> bool:
    """I1 precision gate: a fill is well-typed iff the filler's type matches the
    hole's `hungry_for`.  A `None`/empty filler_type or a hole that accepts any
    type (`hungry_for == "?"`/"") is treated as a typed match for the adapter to
    decide; adapters that know the filler's type should pass it for enforcement.
    """
    if not hole.hungry_for or hole.hungry_for == "?":
        return True
    if filler_type is None:
        return True
    return filler_type == hole.hungry_for


def witness_text(hole: TypedHole, filler: Any, kind: str, note: str | None) -> str:
    lines = [
        f"Witnessed fill for M-typed-holes I5 (projection: {kind}).",
        "",
        f"hole: `{hole.id}`  hungry-for: `{hole.hungry_for}`",
        f"filler: `{filler}`",
    ]
    if hole.source:
        lines.append(f"source: `{json.dumps(hole.source, sort_keys=True)}`")
    if note:
        lines += ["", note]
    lines += ["", "This ArSE answer is the witness record for the fill (I3/I5)."]
    return "\n".join(lines)


def fill(
    hole: TypedHole,
    filler: Any,
    *,
    kind: str | None = None,
    filler_type: str | None = None,
    note: str | None = None,
    author: str = "fill",
    tags: Sequence[str] = DEFAULT_TAGS,
    enforce_type: bool = False,
) -> FillRecord:
    """Discharge `hole` with `filler`, recording a witnessed pair in ArSE.

    THE single runtime fill (I3).  Every projection routes its discharge here.
    - I1: if `enforce_type`, raise unless `type_matches(hole, filler_type)`.
    - I2: a `None`/empty filler records an OPEN hole (no silent drop) and returns
      `filled=False` with no ArSE write (nothing is witnessed for an open hole).
    - I5: a real filler writes `/arse/ask` (the hole) + `/arse/answer` (the
      filler) and returns the evidence IDs as the witness.
    """
    kind = kind or hole.projection
    if kind not in KINDS:
        raise ValueError(f"unknown discharge kind {kind!r}; expected one of {sorted(KINDS)}")

    if filler is None or filler == "" or filler == "?":
        return FillRecord(
            hole_id=hole.id, kind=kind, filled=False, filler=None,
            thread_id=None, question_evidence=None, answer_evidence=None,
            open_note=f"open hole (hungry-for {hole.hungry_for})",
        )

    if enforce_type and not type_matches(hole, filler_type):
        raise ValueError(
            f"I1 precision gate: filler type {filler_type!r} does not match "
            f"hole {hole.id!r} hungry-for {hole.hungry_for!r}"
        )

    ask_res = _http_json("POST", "/ask", {
        "title": f"[{kind}] fill hole {hole.id}",
        "question": f"What fills hole `{hole.id}` (hungry-for `{hole.hungry_for}`)? "
                    f"Projection: {kind}.",
        "author": author,
        "tags": list(tags),
    })
    if not ask_res.get("ok"):
        raise RuntimeError(f"arse/ask failed: {ask_res}")
    thread_id = ask_res["thread-id"]

    answer_res = _http_json("POST", "/answer", {
        "thread-id": thread_id,
        "answer": witness_text(hole, filler, kind, note),
        "author": author,
    })
    if not answer_res.get("ok"):
        raise RuntimeError(f"arse/answer failed: {answer_res}")

    return FillRecord(
        hole_id=hole.id, kind=kind, filled=True, filler=filler,
        thread_id=thread_id,
        question_evidence=ask_res.get("evidence-id"),
        answer_evidence=answer_res.get("evidence-id"),
    )
