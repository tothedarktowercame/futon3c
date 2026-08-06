#!/usr/bin/env python3
"""Statement-bank campaign: formalize APM statements via the local Codex pool.

Reuses the apm-driver modules: agency (dispatch/poll), gates (build, sorry
count, statement extraction/hash). Statements only — the prove loop comes
later and selects from the approved manifest.

Flow per batch: select candidates -> render statement-only packet ->
dispatch to a codex seat -> poll -> statement-gate each problem ->
append manifest entries (pending-review) -> render a review dossier.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

import agency  # noqa: E402
import gates  # noqa: E402

REPO = Path("/home/joe/code/apm-lean")
LEDGER = HERE / "campaign-ledger.jsonl"
MANIFEST = HERE / "statements-manifest.jsonl"
DOSSIER_DIR = HERE / "statement-dossiers"
TEMPLATE = HERE / "templates" / "statement-only.md"
POLL_SECONDS = 60
JOB_TIMEOUT_SECONDS = 3600


def now_iso() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat()


def append_jsonl(path: Path, record: dict) -> None:
    with path.open("a", encoding="utf-8") as fh:
        fh.write(json.dumps(record, sort_keys=True) + "\n")


def candidate_problems() -> list[str]:
    """Problems with a bundle but no formal artifact, informal source present."""

    out = []
    for bundle in sorted(REPO.glob("problems/*")):
        pid = bundle.name
        if (bundle / "lean" / "Main.lean").exists():
            continue
        if not (bundle / "problem.md").exists():
            continue
        out.append(pid)
    return out


def manifest_state() -> dict[str, str]:
    state: dict[str, str] = {}
    if MANIFEST.exists():
        for line in MANIFEST.read_text(encoding="utf-8").splitlines():
            rec = json.loads(line)
            state[rec["problem-id"]] = rec["status"]
    return state


def conclusion_lint(source: str, theorem_name: str) -> list[str]:
    """Advisory vacuity heuristics; never blocks, only flags for review."""

    flags: list[str] = []
    stripped = gates.strip_comments(source)
    m = re.search(rf"theorem\s+{re.escape(theorem_name)}\b(.*?):=\s*by", stripped, re.S)
    if not m:
        return ["lint-unparsed"]
    decl = m.group(1)
    binders = re.findall(r"\(\s*([A-Za-z_][A-Za-z0-9_']*)[^:)]*:\s*[^)]*\)", decl)
    split = decl.rfind(":")
    conclusion = decl[split + 1:] if split >= 0 else decl
    named = [b for b in binders if not b.startswith("h")]
    if named and not any(re.search(rf"\b{re.escape(b)}\b", conclusion) for b in named):
        flags.append(f"conclusion-ignores-objects:{','.join(named[:4])}")
    if len(conclusion.strip()) < 12:
        flags.append("conclusion-trivially-short")
    return flags


def statement_gate(problem_id: str) -> dict:
    lean_file = REPO / "problems" / problem_id / "lean" / "Main.lean"
    if not lean_file.exists():
        return {"outcome": "missing", "reasons": ["no-artifact"]}
    source = lean_file.read_text(encoding="utf-8")
    reasons: list[str] = []
    sorries = gates.count_sorries(source)
    if sorries != 1:
        reasons.append(f"sorry-count-{sorries}-not-1")
    expected = f"apm_{problem_id.lower()}"
    try:
        theorem_name, _norm, digest = gates.statement_hash(source, problem_id)
    except gates.GateError as exc:
        return {"outcome": "defective", "reasons": [f"statement: {exc}"], "sorries": sorries}
    if theorem_name.lower() != expected:
        reasons.append(f"theorem-name-{theorem_name}-not-{expected}")
    build = gates._run_lean(lean_file, repo_root=REPO, timeout_seconds=900)
    if build["exit-code"] != 0:
        reasons.append("build-failed")
    lint = conclusion_lint(source, theorem_name)
    outcome = "defective" if reasons else "pending-review"
    return {
        "outcome": outcome,
        "reasons": reasons,
        "lint": lint,
        "sorries": sorries,
        "theorem-name": theorem_name,
        "statement-hash": digest,
        "build-exit": build["exit-code"],
        "stderr-tail": (build.get("stderr-tail") or "")[-400:],
    }


def render_packet(problems: list[str]) -> str:
    template = TEMPLATE.read_text(encoding="utf-8")
    listing = "\n".join(
        f"- `{pid}` — {(REPO / 'problems' / pid / 'problem.md').as_posix()}"
        for pid in problems
    )
    return template.replace("{problem_list}", "\n" + listing)


def render_dossier(batch_id: str, results: dict[str, dict]) -> Path:
    DOSSIER_DIR.mkdir(exist_ok=True)
    lines = [f"# Statement dossier — {batch_id}", ""]
    lines.append("Review each: source TeX vs Lean statement. Verdict per problem:")
    lines.append("`approve` / `flag <reason>` (edit statements-manifest.jsonl or use")
    lines.append("the campaign `review` subcommand).")
    for pid, res in sorted(results.items()):
        lines.append(f"\n## {pid} — gate: {res['outcome']}")
        if res.get("reasons"):
            lines.append(f"**Gate reasons:** {res['reasons']}")
        if res.get("lint"):
            lines.append(f"**Lint flags (advisory):** {res['lint']}")
        prob = REPO / "problems" / pid / "problem.md"
        tex = prob.read_text(encoding="utf-8") if prob.exists() else ""
        m = re.search(r"```tex(.*?)```", tex, re.S)
        lines.append("\n### Source TeX\n```tex" + (m.group(1) if m else "NOT FOUND") + "```")
        lean_file = REPO / "problems" / pid / "lean" / "Main.lean"
        if lean_file.exists():
            src = lean_file.read_text(encoding="utf-8")
            tm = re.search(r"(theorem\s.*?:=\s*by\b)", gates.strip_comments(src), re.S)
            header = re.search(r"## Statement repairs(.*?)(?:\n#|\Z)", src, re.S)
            lines.append("\n### Lean statement\n```lean\n" + (tm.group(1) if tm else src[:800]) + "\n```")
            if header:
                lines.append("\n### Declared repairs\n" + header.group(1).strip()[:600])
    path = DOSSIER_DIR / f"{batch_id}.md"
    path.write_text("\n".join(lines), encoding="utf-8")
    return path


def run_batch(problems: list[str], seat: str, batch_id: str) -> dict[str, dict]:
    packet = render_packet(problems)
    append_jsonl(LEDGER, {"at": now_iso(), "batch": batch_id, "transition": "dispatch",
                          "seat": seat, "problems": problems})
    job_id = agency.dispatch_fn(seat, packet)["job-id"]
    append_jsonl(LEDGER, {"at": now_iso(), "batch": batch_id, "transition": "job",
                          "job-id": job_id})
    import time
    deadline = time.time() + JOB_TIMEOUT_SECONDS
    while True:
        result = agency.poll_fn(job_id)
        if result.get("status") in ("done", "failed", "error"):
            break
        if time.time() > deadline:
            result = {"status": "campaign-timeout"}
            break
        time.sleep(POLL_SECONDS)
    append_jsonl(LEDGER, {"at": now_iso(), "batch": batch_id, "transition": "poll-done",
                          "status": result.get("status"),
                          "result-tail": str(result.get("result") or "")[-600:]})
    gated: dict[str, dict] = {}
    for pid in problems:
        res = statement_gate(pid)
        gated[pid] = res
        append_jsonl(LEDGER, {"at": now_iso(), "batch": batch_id, "transition": "gate",
                              "problem-id": pid, **res})
        append_jsonl(MANIFEST, {"at": now_iso(), "problem-id": pid,
                                "status": res["outcome"],
                                "statement-hash": res.get("statement-hash"),
                                "batch": batch_id})
    return gated


def cmd_review(args) -> int:
    state = manifest_state()
    if args.problem not in state:
        print(f"{args.problem} not in manifest", file=sys.stderr)
        return 1
    append_jsonl(MANIFEST, {"at": now_iso(), "problem-id": args.problem,
                            "status": "approved" if args.approve else f"flagged:{args.reason}",
                            "reviewer": "claude-10"})
    print(f"{args.problem}: {'approved' if args.approve else 'flagged'}")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    sub = parser.add_subparsers(dest="cmd", required=True)
    runp = sub.add_parser("run")
    runp.add_argument("--n", type=int, default=5)
    runp.add_argument("--batch-size", type=int, default=3)
    runp.add_argument("--seats", default="codex-13,codex-14")
    statusp = sub.add_parser("status")  # noqa: F841
    reviewp = sub.add_parser("review")
    reviewp.add_argument("problem")
    reviewp.add_argument("--approve", action="store_true")
    reviewp.add_argument("--reason", default="")
    args = parser.parse_args()

    if args.cmd == "review":
        return cmd_review(args)
    if args.cmd == "status":
        state = manifest_state()
        from collections import Counter
        print(dict(Counter(state.values())), f"| candidates remaining: {len(candidate_problems())}")
        return 0

    state = manifest_state()
    # missing/defective are RETRYABLE (e.g. a network-killed batch);
    # only settled statuses exclude a problem from selection.
    settled = ("approved", "pending-review")
    todo = [p for p in candidate_problems()
            if not str(state.get(p, "")).startswith(settled)][: args.n]
    if not todo:
        print("nothing to do")
        return 0
    seats = args.seats.split(",")
    batches = [todo[i:i + args.batch_size] for i in range(0, len(todo), args.batch_size)]
    stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%d-%H%M")
    all_results: dict[str, dict] = {}
    import threading
    lock = threading.Lock()

    def work(i: int, chunk: list[str]) -> None:
        seat = seats[i % len(seats)]
        res = run_batch(chunk, seat, f"stmt-{stamp}-{i:02d}")
        with lock:
            all_results.update(res)

    threads = [threading.Thread(target=work, args=(i, c)) for i, c in enumerate(batches)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    dossier = render_dossier(f"stmt-{stamp}", all_results)
    print(f"dossier: {dossier}")
    from collections import Counter
    print(dict(Counter(r["outcome"] for r in all_results.values())))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
