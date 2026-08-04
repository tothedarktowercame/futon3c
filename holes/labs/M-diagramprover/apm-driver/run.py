#!/usr/bin/env python3
"""Compose the APM driver modules into one resumable, single-chain loop."""

from __future__ import annotations

import argparse
import datetime as dt
import importlib.util
import json
import re
import signal
import sys
import time
import uuid
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Mapping

import agency
import checkpoints
import driver
import gates
import render


HERE = Path(__file__).resolve().parent
FUTON_ROOT = HERE.parents[3]
CONFIG = {
    "ledger": HERE / "ledger.jsonl",
    "verdicts": HERE / "verdicts",
    "promotion_queue": HERE / "promotion-queue.jsonl",
    "capability_proof": HERE.parent / "capability-proof.md",
    "candidate_script": FUTON_ROOT / "scripts/apm_formal_zai_cron.py",
    "apm_repo": Path("/home/joe/code/apm-lean"),
    "scribe_root": FUTON_ROOT / "holes/labs/M-zai-learning-loop",
    "agency_base": agency.DEFAULT_AGENCY_BASE,
    "agency_ws": agency.DEFAULT_WS_URL,
    "quota_url": agency.QUOTA_URL,
    "zai_seat": "zai-1",
    "codex_seat": "codex-12",
    "poll_seconds": 60,
    "review_poll_seconds": 60,
    "phase_a_excerpt_chars": 4000,
}

COMMIT_RE = re.compile(r"(?i)\b(?:commit(?:\s+sha)?|sha)\s*[:=]?\s*`?([0-9a-f]{7,40})\b")
MEMORY_RE = re.compile(r"\b(e-[a-z0-9][a-z0-9_-]{5,})\b", re.I)
EVIDENCE_UUID_RE = re.compile(
    r"^e-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$",
    re.I,
)
APPROVAL_RE = re.compile(r"(?m)(/\S*APPROVALS\.md|\S*APPROVALS\.md)")
UPDATE_MARKER = "## Update log"


class RunError(RuntimeError):
    """A composition contract could not be satisfied."""


def candidate_problem_ids() -> list[str]:
    """Load the cron's canonical candidate ordering without copying policy."""

    path = Path(CONFIG["candidate_script"])
    spec = importlib.util.spec_from_file_location("apm_formal_zai_cron", path)
    if spec is None or spec.loader is None:
        raise RunError(f"cannot load candidate selector: {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return list(module.candidate_problem_ids())


def chain_records(ledger_path: Path, chain_id: str) -> list[dict[str, Any]]:
    return [record for record in driver.read_ledger(ledger_path) if record["chain-id"] == chain_id]


def latest_payload(records: list[Mapping[str, Any]], transition: str) -> dict[str, Any]:
    for record in reversed(records):
        if record["transition"] == transition:
            return dict(record["payload"])
    return {}


def result_for_job(records: list[Mapping[str, Any]], job_id: str) -> str:
    for record in reversed(records):
        payload = record["payload"]
        if record["transition"] == "poll" and payload.get("job-id") == job_id:
            return str(payload.get("result") or "")
    return ""


def commit_shas(records: list[Mapping[str, Any]]) -> list[str]:
    found: list[str] = []
    for record in records:
        if record["transition"] != "poll":
            continue
        for match in COMMIT_RE.finditer(str(record["payload"].get("result") or "")):
            sha = match.group(1)
            if sha not in found:
                found.append(sha)
    return found


def main_lean_path(problem_id: str) -> Path:
    return Path("problems") / problem_id / "lean" / "Main.lean"


def source_text(problem_id: str, repo_root: Path | None = None) -> str:
    path = Path(repo_root or CONFIG["apm_repo"]) / main_lean_path(problem_id)
    return path.read_text(encoding="utf-8")


def lean_statement_verbatim(problem_id: str, repo_root: Path | None = None) -> str:
    source = source_text(problem_id, repo_root)
    stripped = gates.strip_comments(source)
    match = gates.THEOREM_RE.search(stripped)
    if match is None:
        raise RunError(f"no main theorem in {main_lean_path(problem_id)}")
    end = stripped.find(":=", match.end())
    if end < 0:
        raise RunError(f"main theorem has no := in {main_lean_path(problem_id)}")
    return source[match.start() : end + 2].strip()


def boundary_excerpt(
    problem_id: str,
    gate_payload: Mapping[str, Any],
    repo_root: Path | None = None,
) -> str:
    """Use the gate scan's sorry-site line and relay its preceding source."""

    sites = gate_payload.get("gate-results", {}).get("boundary-sites", [])
    if not sites:
        return "(gate reported no boundary site)"
    line = int(sites[0]["line"])
    lines = source_text(problem_id, repo_root).splitlines()
    return "\n".join(lines[max(0, line - 11) : line - 1]).strip()


def informal_paths(problem_id: str, repo_root: Path | None = None) -> list[str]:
    repo = Path(repo_root or CONFIG["apm_repo"])
    bundle = repo / "problems" / problem_id
    return [
        str(bundle / "problem.md"),
        str(bundle / "informal-solution.md"),
        str(repo / "apm" / f"{problem_id}.tex"),
    ]


def append_capability_update(
    path: Path,
    *,
    chain_id: str,
    problem_id: str,
    outcome: str,
    hops: int,
    shas: list[str],
    today: dt.date | None = None,
) -> dict[str, Any]:
    """Append one idempotent update-log bullet; never alter another section."""

    text = path.read_text(encoding="utf-8")
    marker_at = text.find(UPDATE_MARKER)
    if marker_at < 0:
        message = f"capability update skipped: missing marker {UPDATE_MARKER!r} in {path}"
        print(message, file=sys.stderr)
        return {"updated": False, "reason": "missing-update-log-marker"}
    after = marker_at + len(UPDATE_MARKER)
    next_section = re.search(r"(?m)^##\s+", text[after:])
    section_end = after + next_section.start() if next_section else len(text)
    if re.search(rf"(?m)^- .*\b{re.escape(chain_id)}\b", text[after:section_end]):
        return {"updated": False, "reason": "already-recorded"}
    stamp = (today or dt.datetime.now(dt.timezone.utc).date()).isoformat()
    certificates = ", ".join(shas) if shas else "none reported"
    bullet = (
        f"- {stamp} — chain {chain_id}; problem {problem_id}; outcome {outcome}; "
        f"hops {hops}; commits {certificates}.\n"
    )
    if next_section:
        prefix = text[:section_end].rstrip() + "\n" + bullet + "\n"
        path.write_text(prefix + text[section_end:].lstrip("\n"), encoding="utf-8")
    else:
        with path.open("a", encoding="utf-8") as stream:
            if text and not text.endswith("\n"):
                stream.write("\n")
            stream.write(bullet)
    return {"updated": True, "bullet": bullet.rstrip()}


def _append(ledger: Path, chain: Mapping[str, Any], transition: str, payload: Mapping[str, Any]) -> None:
    driver.append_transition(
        ledger,
        driver.make_record(chain["chain-id"], chain["problem-id"], transition, payload),
    )


def _promotion_values(result: str, chain_id: str) -> tuple[list[str], str]:
    memory_ids = list(
        dict.fromkeys(
            match.group(1)
            for match in MEMORY_RE.finditer(result)
            if not EVIDENCE_UUID_RE.fullmatch(match.group(1))
        )
    )
    approval = APPROVAL_RE.search(result)
    if not memory_ids or approval is None:
        raise RunError(f"scribe {chain_id} did not report memory ids and APPROVALS.md")
    return memory_ids, approval.group(1)


@dataclass
class Dependencies:
    dispatch: Callable[..., Mapping[str, Any]] = agency.dispatch_fn
    poll: Callable[..., Mapping[str, Any]] = agency.poll_fn
    gate: Callable[..., Mapping[str, Any]] = gates.gate_fn
    renderer: Callable[..., str] = render.render
    quota: Callable[[], Any] = agency.fetch_and_enforce_quota
    checkpoint_request: Callable[..., Mapping[str, Any]] = checkpoints.request_checkpoint
    checkpoint_watch: Callable[..., Mapping[str, Any]] = checkpoints.watch_verdict
    promotion_queue: Callable[..., Mapping[str, Any]] = checkpoints.queue_promotion_approval
    sleep: Callable[[float], None] = time.sleep
    statement: Callable[[str], str] = lean_statement_verbatim
    boundary: Callable[[str, Mapping[str, Any]], str] = boundary_excerpt
    capability_update: Callable[..., Mapping[str, Any]] = append_capability_update


def production_dependencies(config: Mapping[str, Any]) -> Dependencies:
    """Bind each owner module to the deployment values in ``CONFIG``."""

    repo = Path(config["apm_repo"])
    base = str(config["agency_base"])
    return Dependencies(
        dispatch=lambda seat, packet: agency.dispatch_fn(seat, packet, base_url=base),
        poll=lambda job_id: agency.poll_fn(job_id, base_url=base),
        gate=lambda problem_id: gates.gate_fn(problem_id, repo_root=repo),
        quota=lambda: agency.fetch_and_enforce_quota(url=str(config["quota_url"])),
        statement=lambda problem_id: lean_statement_verbatim(problem_id, repo),
        boundary=lambda problem_id, payload: boundary_excerpt(problem_id, payload, repo),
    )


class Runner:
    def __init__(self, deps: Dependencies | None = None, *, config: Mapping[str, Any] | None = None):
        self.config = {**CONFIG, **dict(config or {})}
        self.deps = deps or production_dependencies(self.config)
        self.ledger = Path(self.config["ledger"])

    def _state(self, chain_id: str) -> dict[str, Any]:
        return driver.fold_ledger(driver.read_ledger(self.ledger))[chain_id]

    def _dispatch(self, seat: str, packet: str) -> dict[str, Any]:
        self.deps.quota()
        return dict(self.deps.dispatch(seat, packet))

    def _poll_once(self, chain: Mapping[str, Any]) -> None:
        reply = dict(self.deps.poll(chain["job-id"]))
        _append(self.ledger, chain, "poll", {"job-id": chain["job-id"], **reply})
        if reply["status"] in {"queued", "running"}:
            self.deps.sleep(float(self.config["poll_seconds"]))

    def _render(self, name: str, params: dict[str, str]) -> str:
        return self.deps.renderer(name, params)

    def start_or_resume(self, problem_id: str) -> str:
        chains = driver.fold_ledger(driver.read_ledger(self.ledger))
        active = [c for c in chains.values() if c["problem-id"] == problem_id and c["state"] != "DONE"]
        if active:
            return sorted(active, key=lambda c: c["started-at"])[-1]["chain-id"]
        chain_id = f"apm-{problem_id}-{uuid.uuid4().hex[:12]}"
        driver.append_transition(self.ledger, driver.make_record(chain_id, problem_id, "select", {}))
        return chain_id

    def run_chain(self, problem_id: str) -> dict[str, Any]:
        chain_id = self.start_or_resume(problem_id)
        for _ in range(10000):
            chain = self._state(chain_id)
            records = chain_records(self.ledger, chain_id)
            state = chain["state"]
            bundle = Path(self.config["apm_repo"]) / "problems" / problem_id
            lean_rel = main_lean_path(problem_id)

            if state == "SELECT":
                packet = self._render("phase-a", {"problem_id": problem_id, "bundle_path": str(bundle)})
                dispatched = self._dispatch(str(self.config["zai_seat"]), packet)
                _append(self.ledger, chain, "dispatch-a", dispatched)
            elif state == "DISPATCH_A":
                if chain["last-poll-status"] == "done":
                    report = result_for_job(records, chain["job-id"])
                    base = self._render("phase-b", {
                        "problem_id": problem_id,
                        "bundle_path": str(bundle),
                        "main_lean_path": str(lean_rel),
                    })
                    excerpt = report[-int(self.config["phase_a_excerpt_chars"]):]
                    packet = base + "\n\n--- PHASE A REPORT EXCERPT ---\n" + excerpt
                    dispatched = self._dispatch(str(self.config["zai_seat"]), packet)
                    _append(self.ledger, chain, "dispatch-b", dispatched)
                else:
                    self._poll_once(chain)
            elif state in {"DISPATCH_B", "CLOSER_HOP"}:
                if chain["last-poll-status"] in driver.TERMINAL_POLL_STATUSES:
                    payload = dict(self.deps.gate(problem_id))
                    _append(self.ledger, chain, "gate", payload)
                else:
                    self._poll_once(chain)
            elif state == "GATE":
                payload = dict(self.deps.gate(problem_id))
                _append(self.ledger, chain, "gate", payload)
            elif state == "PARTIAL":
                gate_payload = latest_payload(records, "gate")
                shas = commit_shas(records)
                packet = self._render("closer", {
                    "hop_n": str(chain["hops"] + 1),
                    "problem_id": problem_id,
                    "main_lean_path": str(lean_rel),
                    "base_commit": shas[-1] if shas else "unreported",
                    "sorry_count": str(gate_payload.get("gate-results", {}).get("sorries", "unknown")),
                    "boundary_excerpt": self.deps.boundary(problem_id, gate_payload),
                    "statement_hash": str(chain["statement-hash"]),
                })
                dispatched = self._dispatch(str(self.config["codex_seat"]), packet)
                _append(self.ledger, chain, "closer-hop", {
                    **dispatched,
                    "hop": chain["hops"] + 1,
                    "statement-hash": chain["statement-hash"],
                })
            elif state in {"CLOSED", "DEFECTIVE", "VOID"}:
                if state == "CLOSED" and chain["fidelity-approved"]:
                    shas = commit_shas(records)
                    packet = self._render("scribe", {
                        "problem_id": problem_id,
                        "session_jobs": ", ".join(
                            str(r["payload"].get("job-id")) for r in records
                            if r["transition"] in {"dispatch-a", "dispatch-b", "closer-hop"}
                        ),
                        "commit_sha": shas[-1] if shas else "unreported",
                        "output_dir": str(Path(self.config["scribe_root"]) / f"{problem_id}-scribe"),
                    })
                    dispatched = self._dispatch(str(self.config["codex_seat"]), packet)
                    _append(self.ledger, chain, "scribe", dispatched)
                else:
                    checkpoint = "fidelity" if state == "CLOSED" else "anomaly"
                    resume_state = None if checkpoint == "fidelity" else "GATE"
                    self.deps.quota()
                    self.deps.checkpoint_request(
                        chain_id=chain_id,
                        problem_id=problem_id,
                        checkpoint=checkpoint,
                        lean_statement=self.deps.statement(problem_id),
                        informal_paths=informal_paths(problem_id, Path(self.config["apm_repo"])),
                        gate_results=latest_payload(records, "gate").get("gate-results", {}),
                        ledger_path=self.ledger,
                        verdicts_dir=Path(self.config["verdicts"]),
                        dispatch=self.deps.dispatch,
                        resume_state=resume_state,
                    )
                    if checkpoint == "anomaly":
                        return self._state(chain_id)
            elif state == "AWAITING_REVIEW":
                if chain["review-checkpoint"] == "anomaly":
                    return chain
                watched = dict(self.deps.checkpoint_watch(
                    chain_id=chain_id,
                    checkpoint="fidelity",
                    ledger_path=self.ledger,
                    verdicts_dir=Path(self.config["verdicts"]),
                ))
                if watched["status"] == "error":
                    raise RunError(f"malformed fidelity verdict: {watched.get('error')}")
                if watched["status"] != "applied":
                    self.deps.sleep(float(self.config["review_poll_seconds"]))
            elif state == "SCRIBE":
                if chain["last-poll-status"] == "done":
                    result = result_for_job(records, chain["job-id"])
                    memory_ids, approvals_file = _promotion_values(result, chain_id)
                    queue_path = Path(self.config["promotion_queue"])
                    existing = queue_path.read_text(encoding="utf-8") if queue_path.exists() else ""
                    if not any(json.loads(line).get("chain-id") == chain_id for line in existing.splitlines() if line.strip()):
                        self.deps.promotion_queue(
                            chain_id=chain_id,
                            memory_ids=memory_ids,
                            approvals_file=approvals_file,
                            queue_path=queue_path,
                        )
                    _append(self.ledger, chain, "promotion-queued", {
                        "memory-ids": memory_ids,
                        "approvals-file": approvals_file,
                    })
                else:
                    self._poll_once(chain)
            elif state in {"PROMOTION_QUEUE", "OPEN_HOLE"}:
                shas = commit_shas(records)
                update = self.deps.capability_update(
                    Path(self.config["capability_proof"]),
                    chain_id=chain_id,
                    problem_id=problem_id,
                    outcome=str(chain["outcome"]),
                    hops=int(chain["hops"]),
                    shas=shas,
                )
                _append(self.ledger, chain, "capability-update", {"result": dict(update), "commit-shas": shas})
            elif state == "UPDATE":
                _append(self.ledger, chain, "chain-close", {"outcome": chain["outcome"]})
            elif state == "DONE":
                return chain
            else:
                raise RunError(f"unhandled chain state: {state}")
        raise RunError(f"chain {chain_id} exceeded composition step limit")

    def dry_run(self, problem_id: str) -> None:
        bundle = Path(self.config["apm_repo"]) / "problems" / problem_id
        packets = [
            ("phase-a", {"problem_id": problem_id, "bundle_path": str(bundle)}),
            ("phase-b", {"problem_id": problem_id, "bundle_path": str(bundle), "main_lean_path": str(main_lean_path(problem_id))}),
            ("closer", {"hop_n": "1", "problem_id": problem_id, "main_lean_path": str(main_lean_path(problem_id)), "base_commit": "DRY-RUN", "sorry_count": "unknown", "boundary_excerpt": "(available after mechanical gate)", "statement_hash": "DRY-RUN"}),
            ("scribe", {"problem_id": problem_id, "session_jobs": "DRY-RUN", "commit_sha": "DRY-RUN", "output_dir": str(Path(self.config["scribe_root"]) / f"{problem_id}-scribe")}),
        ]
        for name, params in packets:
            print(f"\n===== {name} =====\n{self._render(name, params)}")


def select_problem(
    explicit: str | None,
    *,
    ledger_path: Path | None = None,
    excluded: set[str] | None = None,
) -> str:
    if explicit:
        return explicit
    skipped = excluded or set()
    if ledger_path is not None:
        chains = driver.fold_ledger(driver.read_ledger(ledger_path))
        active = sorted(
            (chain for chain in chains.values() if chain["state"] != "DONE"),
            key=lambda chain: chain["started-at"],
        )
        for chain in active:
            if chain["problem-id"] not in skipped:
                return str(chain["problem-id"])
    candidates = [problem for problem in candidate_problem_ids() if problem not in skipped]
    if not candidates:
        raise RunError("candidate queue is empty")
    return candidates[0]


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--once", action="store_true")
    mode.add_argument("--continuous", action="store_true")
    parser.add_argument("--problem")
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(argv)
    runner = Runner()
    if args.dry_run:
        runner.dry_run(select_problem(args.problem, ledger_path=runner.ledger))
        return 0

    stopped = False
    def stop(_signum: int, _frame: Any) -> None:
        nonlocal stopped
        stopped = True
    signal.signal(signal.SIGINT, stop)
    with agency.AgencyIdentity(
        base_url=str(CONFIG["agency_base"]),
        ws_url=str(CONFIG["agency_ws"]),
    ):
        if args.once:
            result = runner.run_chain(select_problem(args.problem, ledger_path=runner.ledger))
            print(json.dumps(result, indent=2, sort_keys=True))
            return 0
        handled: set[str] = set()
        while not stopped:
            try:
                problem = select_problem(
                    args.problem,
                    ledger_path=runner.ledger,
                    excluded=handled,
                )
                runner.run_chain(problem)
                handled.add(problem)
            except agency.GateClosed as exc:
                print(f"quota gate closed: {exc}", file=sys.stderr)
                break
            if args.problem:
                break
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
