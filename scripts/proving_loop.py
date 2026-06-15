#!/usr/bin/env python3
"""ArSE-witnessed proving loop for M-typed-holes gate-b.

The runner poses a bounded set of ScopeQuery grounding scopes over the
substrate-2a hx/ store, then records each answer in ArSE.  The ArSE question and
answer evidence IDs are the runtime witness for I5: proof = witnessed fill.

Stdlib only.  ScopeQuery semantics are imported from scope_query_dogfood.py.
"""
from __future__ import annotations

import importlib.util
import json
import sys
import urllib.error
import urllib.request
from dataclasses import dataclass
from pathlib import Path
from typing import Any


ARSE_BASE = "http://localhost:7070/api/alpha/arse"
MANIFEST = Path("/home/joe/code/storage/arse/manifest.json")
STORE_DIR = Path("/home/joe/code/futon6/data/substrate-2a/hx")
REPORT = Path("/home/joe/code/futon3c/scripts/proving-loop-report.md")
AUTHOR = "proving-loop"
TAGS = ["m-typed-holes", "substrate-2a"]


@dataclass(frozen=True)
class Scope:
    paper: str
    subject: str
    title: str

    @property
    def store_path(self) -> Path:
        return STORE_DIR / f"{self.paper}.edn"

    @property
    def scope_text(self) -> str:
        return f"{{:kind :bind (:subject :{self.subject}) (:concept ?c)}}"

    @property
    def question(self) -> str:
        return (
            f"Which concept grounds symbol `{self.subject}` in substrate-2a "
            f"paper `{self.paper}`? Scope: `{self.scope_text}`. "
            "Answer by running ScopeQuery.answers over the imported hx/ store."
        )


SCOPES = [
    Scope("0704.0502", "A", "substrate-2a: concept grounding for 0704.0502/A"),
    Scope("0704.0502", "E", "substrate-2a: concept grounding for 0704.0502/E"),
    Scope("0705.0452", "G", "substrate-2a: concept grounding for 0705.0452/G"),
    Scope("0705.0452", "K", "substrate-2a: concept grounding for 0705.0452/K"),
    Scope("0708.2659", "Set", "substrate-2a: concept grounding for 0708.2659/Set"),
]


def load_scopequery():
    path = Path(__file__).with_name("scope_query_dogfood.py")
    spec = importlib.util.spec_from_file_location("scope_query_dogfood", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def read_manifest_count() -> int | None:
    if not MANIFEST.exists():
        return None
    data = json.loads(MANIFEST.read_text())
    count = data.get("entity_count")
    return int(count) if isinstance(count, int) else None


def http_json(method: str, path: str, payload: dict[str, Any] | None = None) -> dict[str, Any]:
    data = None
    headers = {}
    if payload is not None:
        data = json.dumps(payload).encode("utf-8")
        headers["Content-Type"] = "application/json"
    req = urllib.request.Request(
        f"{ARSE_BASE}{path}",
        data=data,
        headers=headers,
        method=method,
    )
    try:
        with urllib.request.urlopen(req, timeout=20) as res:
            body = res.read().decode("utf-8")
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise RuntimeError(f"{method} {path} failed: HTTP {exc.code}: {detail}") from exc
    return json.loads(body)


def ask(scope: Scope) -> dict[str, Any]:
    return http_json(
        "POST",
        "/ask",
        {
            "title": scope.title,
            "question": scope.question,
            "author": AUTHOR,
            "tags": TAGS,
        },
    )


def answer(thread_id: str, answer_text: str) -> dict[str, Any]:
    return http_json(
        "POST",
        "/answer",
        {
            "thread-id": thread_id,
            "answer": answer_text,
            "author": AUTHOR,
        },
    )


def unanswered_thread_ids() -> set[str]:
    data = http_json("GET", "/unanswered")
    out = set()
    for q in data.get("questions", []):
        if isinstance(q, dict):
            tid = q.get("thread-id") or q.get("thread_id") or q.get("id")
            if isinstance(tid, str):
                out.add(tid)
    return out


def compute_witness(scopequery, scope: Scope) -> dict[str, Any]:
    store = scopequery.load_store(scope.store_path)
    bindings = scopequery.answers(
        store,
        kind="bind",
        bound=[("subject", scope.subject)],
        holes=[("concept", "c")],
    )
    raw = [binding["c"] for binding in bindings if "c" in binding]
    fills = sorted({value for value in raw if value != "?"})
    open_count = sum(1 for value in raw if value == "?")
    if not raw:
        raise RuntimeError(f"scope produced no answers: {scope.scope_text}")
    return {
        "store_edges": len(store),
        "raw_count": len(raw),
        "fills": fills,
        "open_count": open_count,
    }


def answer_text(scope: Scope, witness: dict[str, Any]) -> str:
    fill_lines = "\n".join(f"- `{fill}`" for fill in witness["fills"])
    if not fill_lines:
        fill_lines = "- none"
    return "\n".join(
        [
            "Witnessed fill for M-typed-holes I5.",
            "",
            f"paper: `{scope.paper}`",
            f"store: `{scope.store_path}`",
            f"scope: `{scope.scope_text}`",
            f"ScopeQuery.answers raw binding count: {witness['raw_count']}",
            "",
            "Filled concepts, excluding the open-hole sentinel `?`:",
            fill_lines,
            "",
            f"Open-hole sentinel count: {witness['open_count']}",
            "",
            "This ArSE answer is the witness record for the ScopeQuery fill.",
        ]
    )


def render_report(rows: list[dict[str, Any]], before: int | None, after: int | None) -> str:
    lines = [
        "# proving-loop report",
        "",
        "Runner: `scripts/proving_loop.py`",
        "",
        f"- ArSE manifest before entity_count: {before if before is not None else 'unavailable'}",
        f"- ArSE manifest after entity_count: {after if after is not None else 'unavailable'}",
        f"- scopes posted: {len(rows)}",
        "- total ArSE writes: 10 (5 asks + 5 answers)",
        "",
        "| paper | scope | thread-id | question evidence | answer evidence | witnessed fills | open holes |",
        "|---|---|---|---|---|---|---:|",
    ]
    for row in rows:
        fills = ", ".join(f"`{fill}`" for fill in row["fills"]) or "none"
        lines.append(
            f"| {row['paper']} | `{row['scope']}` | `{row['thread_id']}` | "
            f"`{row['question_evidence']}` | `{row['answer_evidence']}` | "
            f"{fills} | {row['open_count']} |"
        )
    lines.extend(
        [
            "",
            "Verification:",
            "",
            "- `?` was filtered out of witnessed fill text and counted separately.",
            "- Every posted thread was absent from `/api/alpha/arse/unanswered` after answering.",
            "- Witnessed fills are exactly `ScopeQuery.answers(... ?c)` values with `?` removed.",
            "",
        ]
    )
    return "\n".join(lines)


def main() -> int:
    if len(SCOPES) * 2 > 10:
        raise RuntimeError("hard bound exceeded: more than 10 ArSE writes")

    scopequery = load_scopequery()
    before = read_manifest_count()
    rows: list[dict[str, Any]] = []

    for scope in SCOPES:
        witness = compute_witness(scopequery, scope)
        ask_res = ask(scope)
        if not ask_res.get("ok"):
            raise RuntimeError(f"ask failed: {ask_res}")
        thread_id = ask_res["thread-id"]
        answer_res = answer(thread_id, answer_text(scope, witness))
        if not answer_res.get("ok"):
            raise RuntimeError(f"answer failed: {answer_res}")
        rows.append(
            {
                "paper": scope.paper,
                "scope": scope.scope_text,
                "thread_id": thread_id,
                "question_evidence": ask_res.get("evidence-id", ""),
                "answer_evidence": answer_res.get("evidence-id", ""),
                "fills": witness["fills"],
                "open_count": witness["open_count"],
            }
        )

    missing = {row["thread_id"] for row in rows} & unanswered_thread_ids()
    if missing:
        raise RuntimeError(f"posted threads still unanswered: {sorted(missing)}")

    after = read_manifest_count()
    if before is not None and after is not None and after - before != len(SCOPES):
        raise RuntimeError(
            f"manifest entity_count delta {after - before} != scopes posted {len(SCOPES)}"
        )

    REPORT.write_text(render_report(rows, before, after))
    print(f"wrote {REPORT}")
    for row in rows:
        print(f"{row['thread_id']} {row['scope']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
