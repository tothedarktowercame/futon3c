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
import os
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

import agency  # noqa: E402
import gates  # noqa: E402

REPO = Path("/home/joe/code/apm-lean")

# The gate shells out to `lake`, which lives in elan's bin. A systemd unit (or
# any non-login shell) does not inherit it, and Zone has no global elan default
# either — so relying on ambient PATH cost a silent skip-storm AND a mid-run
# crash on 2026-08-06. Put it on PATH here rather than in every launch command.
_ELAN_BIN = Path.home() / ".elan" / "bin"
if _ELAN_BIN.is_dir() and str(_ELAN_BIN) not in os.environ.get("PATH", "").split(os.pathsep):
    os.environ["PATH"] = f"{_ELAN_BIN}{os.pathsep}{os.environ.get('PATH', '')}"
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


def citation_lint(source: str) -> list[str]:
    """Memory: markers must carry e- ids; pattern ids belong in Pattern:."""

    flags = []
    for m in re.findall(r"\(Memory: ([^)]+)\)", source):
        if not m.strip().startswith("e-"):
            flags.append(f"memory-marker-carries-non-memory-id:{m.strip()[:40]}")
    for m in re.findall(r"\(Pattern: ([^)]+)\)", source):
        if m.strip().startswith("e-"):
            flags.append(f"pattern-marker-carries-memory-id:{m.strip()[:40]}")
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


def pass1_attempted() -> set[str]:
    """Problems already claimed by a pass-1 run: committed OR still in flight.

    Committed outcomes come from the repo log; in-flight ones come from the
    ledger, because the zai and codex lanes draw from one approved pool and a
    problem dispatched to zai must not also be dealt to codex before it has
    had time to commit.
    """

    import subprocess

    log = subprocess.run(
        ["git", "log", "--pretty=%s"], cwd=REPO, capture_output=True, text=True
    ).stdout
    claimed = {m.group(1) for m in re.finditer(r"^(\S+): pass-1 ", log, re.M)}
    # A commit records what a prover DID; the manifest records what the campaign
    # accepts. When review annuls an outcome (resetting it to "approved" — e.g.
    # the Zone skips produced by a host with no elan default toolchain), the
    # stale commit must stop holding the claim. In-flight ledger claims below
    # are NOT overridable this way: a job still running has status "approved"
    # too, and dropping its claim would double-dispatch it.
    state = manifest_state()
    claimed = {p for p in claimed if state.get(p) != "approved"}
    for pid, job_id in pass1_dispatches():
        if pid in claimed:
            continue
        # A dispatch that died without producing a commit (the 429 bounces
        # when the Z.ai window ran dry) is RETRYABLE, not settled. Only a
        # live or successful job holds the claim.
        try:
            status = agency.poll_fn(job_id).get("status")
        except agency.AgencyError:
            # Job unknown to this Agency: a ledger entry from the other box,
            # or history lost to a JVM restart. It cannot be holding a claim,
            # and anything it finished is caught by the commit scan above.
            continue
        # A cancelled job released its problem as surely as a failed one did;
        # leaving the claim held would strand it (hit 2026-08-06 when a
        # too-large Codex dispatch had to be trimmed to free the zai lane).
        if status not in ("failed", "error", "cancelled"):
            claimed.add(pid)
    return claimed


def pass1_dispatches() -> list[tuple[str, str]]:
    """(problem-id, job-id) for every pass-1 dispatch recorded in the ledger."""

    out: list[tuple[str, str]] = []
    if not LEDGER.exists():
        return out
    for line in LEDGER.read_text(encoding="utf-8").splitlines():
        rec = json.loads(line)
        if rec.get("transition") != "pass1-dispatch":
            continue
        for pid in rec.get("problems") or []:
            out.append((pid, rec.get("job-id")))
    return out


# Only the STATEMENT gate declares what the bank froze. A proving attempt is an
# observation about an artifact, never a redeclaration of the contract.
STATEMENT_ESTABLISHING = frozenset({"pending-review", "approved", "defective", "missing"})


def frozen_hash(problem_id: str) -> str | None:
    """The statement hash the bank froze for this problem.

    Last *statement-establishing* record wins — NOT simply the last record.
    Before this was scoped (2026-08-06), `pass1_gate` wrote the hash it observed
    into the manifest, so detecting a substitution OVERWROTE the reference the
    substitution had violated: re-checking then reported clean, and the detector
    erased its own evidence (seen on a97J07). A proving outcome must never move
    the contract; a repair re-freezes only by passing the statement gate again.
    """

    digest = None
    if MANIFEST.exists():
        for line in MANIFEST.read_text(encoding="utf-8").splitlines():
            rec = json.loads(line)
            if rec["problem-id"] != problem_id or not rec.get("statement-hash"):
                continue
            if rec.get("status") not in STATEMENT_ESTABLISHING:
                continue
            digest = rec["statement-hash"]
    return digest


def frozen_declarations(problem_id: str) -> dict[str, str] | None:
    """The whole reviewed claim set, for artifacts with no single main theorem.

    Same last-statement-establishing-record-wins rule as `frozen_hash`, and for
    the same reason: a proving outcome must never move the contract.
    """

    decls = None
    if MANIFEST.exists():
        for line in MANIFEST.read_text(encoding="utf-8").splitlines():
            rec = json.loads(line)
            if rec["problem-id"] != problem_id or not rec.get("declaration-hashes"):
                continue
            if rec.get("status") not in STATEMENT_ESTABLISHING:
                continue
            decls = rec["declaration-hashes"]
    return decls


def artifact_hash(problem_id: str) -> str:
    """SHA-256 of the artifact exactly as the gate would judge it."""

    import hashlib

    lean_file = REPO / "problems" / problem_id / "lean" / "Main.lean"
    if not lean_file.exists():
        return ""
    return hashlib.sha256(lean_file.read_bytes()).hexdigest()


def last_gated_hash(problem_id: str) -> str:
    """Content hash at this problem's most recent pass-1 gate, if any."""

    seen = ""
    if LEDGER.exists():
        for line in LEDGER.read_text(encoding="utf-8").splitlines():
            rec = json.loads(line)
            if rec.get("transition") == "pass1-gate" \
               and rec.get("problem-id") == problem_id and rec.get("content-hash"):
                seen = rec["content-hash"]
    return seen


def pass1_gate(problem_id: str) -> dict:
    """Classify a pass-1 outcome and check the statement never moved.

    The frozen-statement guarantee is only real if something checks it, so
    the hash comparison comes FIRST: a changed statement is void regardless
    of how clean the build is (the a98A01 failure mode).
    """

    lean_file = REPO / "problems" / problem_id / "lean" / "Main.lean"
    if not lean_file.exists():
        return {"outcome": "missing", "reasons": ["no-artifact"]}
    source = lean_file.read_text(encoding="utf-8")
    # Set contract first: it is the only contract the 120 pre-campaign artifacts
    # have (no theorem is named for the problem, so there is no "main" statement
    # to hash), and it is strictly the stronger check where both exist.
    frozen_decls = frozen_declarations(problem_id)
    if frozen_decls:
        drift = gates.declaration_set_drift(frozen_decls, source)
        if drift:
            return {"outcome": "void-statement-changed",
                    "reasons": ["declaration-set-drift"] + drift[:6],
                    "declaration-drift": drift}
    theorem_name = None
    try:
        theorem_name, _norm, digest = gates.statement_hash(source, problem_id)
    except gates.GateError as exc:
        if not frozen_decls:
            return {"outcome": "defective", "reasons": [f"statement: {exc}"]}
        digest = None
    expected = frozen_hash(problem_id)
    if expected and digest and digest != expected:
        return {"outcome": "void-statement-changed", "reasons": ["hash-moved"],
                "statement-hash": digest, "frozen-hash": expected}
    sorries = gates.count_sorries(source)
    build = gates._run_lean(lean_file, repo_root=REPO, timeout_seconds=900)
    declared = declared_outcome(problem_id)
    reasons: list[str] = []
    # The prover DECLARES its outcome in the commit message; the gate's job is
    # to verify that claim mechanically, not to re-derive it from heuristics.
    # A claim the artifact does not support is downgraded, never accepted.
    if build["exit-code"] != 0:
        outcome = "defective"
        reasons.append("build-failed")
    elif declared == "proved" and sorries > 0:
        outcome = "partial"
        reasons.append(f"claimed-proved-but-{sorries}-sorries")
    elif declared == "partial" and sorries == 0:
        outcome = "proved"
        reasons.append("claimed-partial-but-zero-sorries")
    elif declared in ("proved", "partial", "skipped"):
        outcome = declared
    elif sorries == 0:
        outcome = "proved"
    else:
        outcome = "partial"
        reasons.append("no-declared-outcome")
    # A zero-sorry build that compiles says nothing about WHAT closed it. On
    # 2026-08-08 this gate logged `b95J01: proved` for an artifact resting on
    # seven `_native.native_decide.ax_*` axioms — i.e. trusted to the compiler,
    # not checked by the kernel. Three closes reached `proved` that way
    # (b97J01, b00J02, b95J01) before a separate sweep caught them.
    #
    # Probe only on `proved`: it is the sole outcome the check can change, and
    # it keeps the extra elaboration off every partial hop.
    impure: list[str] = []
    # Cheaper than the axiom probe and catches what the probe cannot: `opaque`
    # adds no axioms, so a statement resting on uninterpreted constants passes
    # both the sorry count and the sweep while not being about its own subject.
    placeholders = gates.opaque_declarations(source)
    if outcome == "proved" and placeholders:
        outcome = "defective"
        reasons.append("opaque-placeholders: " + ",".join(placeholders[:4]))
    if outcome == "proved":
        if not theorem_name:
            # Pre-campaign artifacts name no theorem for the problem, so there
            # is nothing to probe. Say so rather than implying a clean result.
            reasons.append("axiom-probe-skipped-no-theorem")
        else:
            # Probe the problem's OWN theorem, not whichever claim the
            # statement regex matched first. t97J01 names its helper
            # `apm_t97J01_surjective_of_connected` and its main theorem
            # `apm_t97j01` (different case), so the regex returns the helper —
            # and a clean helper would have certified a dirty main theorem.
            # Selection only; the frozen hash is deliberately left alone,
            # since changing it would re-key every banked statement.
            probe_name = theorem_name
            wanted = f"apm_{problem_id}".lower()
            for claim in gates.declaration_hashes(source):
                if claim.lower() == wanted:
                    probe_name = claim
                    break
            probe = gates.run_axiom_probe(source, probe_name, repo_root=REPO,
                                          timeout_seconds=900)
            impure = probe["impure"]
            if impure:
                outcome = "defective"
                reasons.append("impure-axioms: " + ",".join(impure[:6]))
            elif probe["exit-code"] != 0 or probe["line"] is None:
                # Do NOT downgrade on a failed probe: a flaky or timed-out
                # elaboration would silently void real closes. Record it and
                # let review decide; the 2-hourly sweep catches it either way.
                reasons.append("axiom-probe-failed")
    return {"outcome": outcome, "reasons": reasons, "sorries": sorries,
            "declared": declared, "theorem-name": theorem_name,
            "statement-hash": digest, "build-exit": build["exit-code"],
            "impure-axioms": impure,
            "content-hash": artifact_hash(problem_id)}


def declared_outcome(problem_id: str) -> str | None:
    """The outcome the prover claimed for the artifact AS IT STANDS NOW.

    Only the newest commit touching the bundle counts. Scanning the whole log
    (the first version) returned a declaration that a later commit had already
    superseded: on 2026-08-07 the closer did real work on seven m95* problems,
    committed it ("prove Green kernel bounds and symmetry", 19:05), and the gate
    logged every one of them `skipped` — because a pass-1 commit from that
    morning still said `m95J04: pass-1 skipped` and matched first.

    A stale claim is worse than none. It survives the branch that would have
    caught it, too: `pass1_gate` downgrades a claimed-proved with sorries and
    upgrades a claimed-partial without them, but takes `skipped` at its word —
    so a hop that genuinely CLOSED a previously-skipped problem was recorded as
    a skip and left in the queue. When the newest commit carries no declaration,
    returning None is right: the gate then reads the artifact.
    """

    import subprocess

    subject = subprocess.run(
        ["git", "log", "-1", "--pretty=%s", "--", f"problems/{problem_id}/"],
        cwd=REPO, capture_output=True, text=True).stdout.strip()
    m = re.match(rf"^{re.escape(problem_id)}: pass-1 (\w+)", subject)
    return m.group(1) if m else None


def cmd_pass1_gate(args) -> int:
    """Gate every problem that has a pass-1 commit but no recorded outcome."""

    import subprocess

    log = subprocess.run(["git", "log", "--pretty=%s"], cwd=REPO,
                         capture_output=True, text=True).stdout
    committed = {m.group(1) for m in re.finditer(r"^(\S+): pass-1 ", log, re.M)}
    state = manifest_state()
    settled = ("proved", "partial", "skipped", "void-statement-changed",
               "partial-by-review", "defective")
    todo = sorted(p for p in committed if state.get(p) not in settled)
    if args.problems:
        todo = [p for p in args.problems.split(",") if p]
    # Skip anything whose artifact is byte-for-byte what the gate last judged.
    # `settled` does not include `approved`, so every problem I approve was
    # being re-gated on the next cycle — b97J01 was re-elaborated every ten
    # minutes, indefinitely, for the same `partial`. A gate run here is a FULL
    # Mathlib elaboration (problems/ is not a lean_lib, so there are no oleans
    # and nothing is incremental), which is the single most expensive thing
    # this campaign does. Same content, same verdict: don't pay twice.
    if not args.regate:
        fresh = [p for p in todo if artifact_hash(p) != last_gated_hash(p)]
        if len(fresh) != len(todo):
            print("skipping %d unchanged artifact(s): %s"
                  % (len(todo) - len(fresh),
                     " ".join(p for p in todo if p not in fresh)[:200]))
        todo = fresh
    if not todo:
        print("no pass-1 outcomes awaiting the gate")
        return 0
    from collections import Counter
    tally: Counter = Counter()
    for pid in todo:
        res = pass1_gate(pid)
        tally[res["outcome"]] += 1
        print(f"{pid}: {res['outcome']}"
              + (f" ({res['reasons']})" if res.get("reasons") else ""))
        append_jsonl(MANIFEST, {"at": now_iso(), "problem-id": pid,
                                "status": res["outcome"],
                                "observed-statement-hash": res.get("statement-hash"),
                                "gate": "pass1"})
        append_jsonl(LEDGER, {"at": now_iso(), "transition": "pass1-gate",
                              "problem-id": pid, **res})
    print(dict(tally))
    return 0


def quota_wait_seconds(*, min_available: float, logger=None) -> float:
    """0 if the window is open; otherwise seconds until the earliest reset.

    Joe's instruction (2026-08-06) is to run to exhaustion rather than stop at
    a 50%-available floor, so the floor is a parameter and the drainer's
    default is 0 — the gate then only closes on genuine exhaustion.
    """

    import time

    limits = agency.quota_snapshot(
        agency._response(agency.url_fetch, "GET", agency.QUOTA_URL,
                         headers={"Authorization": agency.api_key(),
                                  "Accept-Language": "en-US,en",
                                  "Content-Type": "application/json"},
                         timeout=30)[1])
    blocked = [l for l in limits if float(l["available"]) <= min_available]
    if logger:
        logger("quota " + ",".join(
            f"unit={l['unit']}/used={l['used']:g}/avail={l['available']:g}" for l in limits))
    if not blocked:
        return 0.0
    soonest = min(float(l["next_reset_ms"]) / 1000.0 for l in blocked)
    return max(60.0, soonest - time.time() + 30.0)


def cmd_pass1_drain(args) -> int:
    """Run pass-1 continuously: dispatch a tranche, gate it, repeat.

    Sleeps out a closed quota window rather than exiting, because "run
    continuously when usage is available" means resume-when-permitted.
    """

    import time

    def say(msg: str) -> None:
        print(f"[{now_iso()}] {msg}", flush=True)

    seats = args.seats.split(",")
    template = (HERE / "templates" / args.template).read_text(encoding="utf-8")
    deadline = time.time() + args.max_hours * 3600
    tranches = 0
    while time.time() < deadline:
        state = manifest_state()
        attempted = pass1_attempted()
        todo = [p for p, st in sorted(state.items())
                if st == "approved" and p not in attempted][: args.tranche]
        if not todo:
            say("bank drained: no approved-unattempted statements left")
            return 0
        try:
            wait = quota_wait_seconds(min_available=args.min_available, logger=say)
        except agency.GateClosed as exc:
            say(f"quota unreadable ({exc}); retrying in 15m")
            time.sleep(900)
            continue
        if wait > 0:
            say(f"quota exhausted; sleeping {wait/3600:.2f}h for the window")
            time.sleep(wait)
            continue
        stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%d-%H%M")
        jobs = []
        for i, pid in enumerate(todo):
            seat = seats[i % len(seats)]
            job = agency.dispatch_fn(seat, template.replace("{problem_list}", f"\n- `{pid}`"))["job-id"]
            jobs.append((pid, job))
            append_jsonl(LEDGER, {"at": now_iso(), "batch": f"pass1-{stamp}-{i:03d}",
                                  "transition": "pass1-dispatch", "seat": seat,
                                  "problems": [pid], "job-id": job})
        tranches += 1
        say(f"tranche {tranches}: dispatched {len(jobs)} to {','.join(seats)}")
        job_deadline = time.time() + args.tranche_timeout_hours * 3600
        while time.time() < job_deadline:
            states = [agency.poll_fn(j).get("status") for _, j in jobs]
            if all(s in ("done", "failed", "error") for s in states):
                break
            time.sleep(POLL_SECONDS)
        from collections import Counter
        say(f"tranche {tranches} jobs: {dict(Counter(states))}")
        outcomes: Counter = Counter()
        for pid, _ in jobs:
            res = pass1_gate(pid)
            outcomes[res["outcome"]] += 1
            append_jsonl(MANIFEST, {"at": now_iso(), "problem-id": pid,
                                    "status": res["outcome"],
                                    "statement-hash": res.get("statement-hash"),
                                    "gate": "pass1"})
            append_jsonl(LEDGER, {"at": now_iso(), "transition": "pass1-gate",
                                  "problem-id": pid, **res})
        say(f"tranche {tranches} gated: {dict(outcomes)}")
    say(f"max-hours reached after {tranches} tranches")
    return 0


def bundle_touched_since(problem_id: str, iso_ts: str) -> bool:
    """Did a commit touch this problem's bundle after `iso_ts`?

    The only reliable way to tell a real repair from a phantom one: the job
    record says a repair ran, the commit log says whether anything changed.
    """

    import subprocess

    out = subprocess.run(
        ["git", "log", "-1", "--format=%cI", "--", f"problems/{problem_id}/"],
        cwd=REPO, capture_output=True, text=True).stdout.strip()
    return bool(out) and out > iso_ts


def repair_targets() -> list[tuple[str, str]]:
    """(problem-id, why) for statements a reviewer rejected or the gate failed.

    The reviewer's own words travel into the repair packet — the finding IS the
    specification, so the fixer does not have to re-derive what was wrong.
    """

    # (status, note) per problem — the note matters because the flag reason
    # moved there. The first version of this selected on `status.startswith
    #("flagged:")`, i.e. on the compound "flagged:<the whole essay>" that the
    # ad-hoc flag script used to write. Normalising that to status "flagged"
    # plus a `note` (2026-08-07, so the flags would actually be countable)
    # silently emptied this queue: the repair lane restarted 160 times
    # overnight logging "nothing to repair" while twenty flagged statements
    # sat untouched. Accept both spellings; a consumer of a format should not
    # be the last thing anyone remembers to update.
    # A REVIEWER's verdict outranks any later gate outcome. Taking simply the
    # last record let a gate erase a flag: on 2026-08-08 the repair lane ran
    # while the Codex window was exhausted, every job died without editing a
    # file, and `statement_gate` — which only checks sorry count, theorem name
    # and build — passed the UNCHANGED artifact and wrote `pending-review` over
    # five of my flags. They then vanished from this queue permanently and were
    # silently skipped on the next run. Same shape as the frozen_hash fix: a
    # proving outcome must never overwrite a review decision.
    latest: dict[str, tuple[str, str]] = {}
    verdict: dict[str, tuple[str, str, str]] = {}
    repaired_at: dict[str, str] = {}
    if MANIFEST.exists():
        for line in MANIFEST.read_text(encoding="utf-8").splitlines():
            rec = json.loads(line)
            pid = rec["problem-id"]
            latest[pid] = (rec["status"], rec.get("note") or "")
            if rec.get("reviewer"):
                verdict[pid] = (rec["status"], rec.get("note") or "", rec["at"])
            elif rec.get("gate") == "repair":
                repaired_at[pid] = rec["at"]
    # Two failures to avoid at once, and only the ARTIFACT distinguishes them.
    # (1) A phantom job — one that died on quota without editing anything —
    #     still produced a `pending-review` gate record, which silently erased
    #     five flags and dropped them from this queue for good.
    # (2) A genuine repair that is left flagged comes straight back, so the
    #     lane re-repairs the same file forever; the work is done and the
    #     problem is review work now.
    # A repair-gate record alone cannot tell these apart. A commit can: the
    # flag is cleared only if a repair landed AND the bundle actually changed.
    for pid, (status, note, flagged_at) in verdict.items():
        if status != "flagged" and not status.startswith("flagged:"):
            continue
        if repaired_at.get(pid, "") > flagged_at and bundle_touched_since(pid, flagged_at):
            continue
        latest[pid] = (status, note)
    gate_reasons: dict[str, str] = {}
    if LEDGER.exists():
        for line in LEDGER.read_text(encoding="utf-8").splitlines():
            rec = json.loads(line)
            if rec.get("transition") == "gate" and rec.get("reasons"):
                gate_reasons[rec["problem-id"]] = ", ".join(rec["reasons"])
    out = []
    for pid, (status, note) in sorted(latest.items()):
        if status == "flagged" or status.startswith("flagged:"):
            why = note.strip() or status.partition(":")[2].strip()
            out.append((pid, why or "flagged by review; reason not recorded"))
        elif status == "defective":
            why = gate_reasons.get(pid, "the statement gate rejected this artifact")
            out.append((pid, f"The statement gate rejected this artifact: {why}. "
                             "Fix so it builds, contains exactly one sorry, and the "
                             "theorem is named for the problem."))
    return out


def cmd_repair(args) -> int:
    """Dispatch one repair job per rejected statement, then re-gate."""

    import threading
    import time

    targets = repair_targets()
    if args.problems:
        wanted = {p for p in args.problems.split(",") if p}
        targets = [t for t in targets if t[0] in wanted]
    if args.limit:
        targets = targets[: args.limit]
    if not targets:
        print("nothing to repair")
        return 0
    if args.dry_run:
        for pid, why in targets:
            print(f"{pid}: {why[:110]}")
        print(f"({len(targets)} targets)")
        return 0
    template = (HERE / "templates" / "statement-repair.md").read_text(encoding="utf-8")
    seats = args.seats.split(",")
    stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%d-%H%M")
    results: dict[str, dict] = {}
    lock = threading.Lock()

    def work(i: int, pid: str, why: str) -> None:
        seat = seats[i % len(seats)]
        packet = (template.replace("{problem_id_lower}", pid.lower())
                          .replace("{problem_id}", pid)
                          .replace("{flag_reason}", why))
        batch = f"repair-{stamp}-{i:03d}"
        append_jsonl(LEDGER, {"at": now_iso(), "batch": batch, "transition": "repair-dispatch",
                              "seat": seat, "problems": [pid]})
        try:
            job = agency.dispatch_fn(seat, packet)["job-id"]
        except agency.AgencyError as exc:
            print(f"{pid}: dispatch failed ({exc})", flush=True)
            return
        deadline = time.time() + JOB_TIMEOUT_SECONDS
        while time.time() < deadline:
            if agency.poll_fn(job).get("status") in ("done", "failed", "error"):
                break
            time.sleep(POLL_SECONDS)
        res = statement_gate(pid)
        with lock:
            results[pid] = res
        append_jsonl(LEDGER, {"at": now_iso(), "batch": batch, "transition": "repair-gate",
                              "problem-id": pid, **res})
        append_jsonl(MANIFEST, {"at": now_iso(), "problem-id": pid,
                                "status": res["outcome"],
                                "statement-hash": res.get("statement-hash"),
                                "gate": "repair"})
        print(f"{pid}: {res['outcome']}"
              + (f" {res['reasons']}" if res.get("reasons") else ""), flush=True)

    # Bounded concurrency: each in-flight job means a Mathlib elaboration in the
    # agent AND one in this process at gate time. Unbounded fan-out OOM'd the
    # laptop on 2026-08-06.
    queue = list(enumerate(targets))
    threads: list[threading.Thread] = []

    def worker(slot: int) -> None:
        while True:
            with lock:
                if not queue:
                    return
                i, (pid, why) = queue.pop(0)
            work(i, pid, why)

    for slot in range(min(args.concurrency, len(targets))):
        t = threading.Thread(target=worker, args=(slot,))
        t.start()
        threads.append(t)
    for t in threads:
        t.join()
    from collections import Counter
    print(dict(Counter(r["outcome"] for r in results.values())))
    if results:
        print(f"dossier: {render_dossier(f'repair-{stamp}', results)}")
    return 0


MAX_CLOSER_HOPS = 3
# Stop the run rather than march through the queue burning nothing: with
# 18% of the Codex window left, a dead seat must cost a few dispatches, not
# the whole budget's worth of ledger noise.
DISPATCH_FAILURE_LIMIT = 5


# Statuses whose artifact may be handed to a closer: the statement is settled
# and a proof is outstanding. `pending-review` is excluded ON PURPOSE — proving
# against an unreviewed statement is the failure the bank exists to prevent —
# as are `flagged` and `void-statement-changed`, whose statements are known bad.
CLOSER_ELIGIBLE = ("approved", "skipped", "partial", None)

# Fewest sorries and the strongest contract first. A close only counts if the
# statement was reviewed, so `approved` (reviewed + frozen) outranks `skipped`
# (never genuinely attempted — the 2026-08-06 skip-storm residue), which
# outranks a partial whose earlier hops already failed.
_CLOSER_RANK = {"approved": 0, "skipped": 1, None: 2, "partial": 3}

# One problem id per line; blank lines and #-comments ignored. These jump the
# queue ahead of every status rank, so a freshly promoted ConstructionTarget can
# be tested against the problems it was built for while the evidence is fresh —
# rather than waiting for the ordinary sweep to reach them hours later.
PRIORITY_FILE = HERE / "closer-priority.txt"


def closer_priority() -> list[str]:
    if not PRIORITY_FILE.exists():
        return []
    out = []
    for line in PRIORITY_FILE.read_text(encoding="utf-8").splitlines():
        line = line.split("#", 1)[0].strip()
        if line:
            out.append(line)
    return out


def closer_targets() -> list[tuple[str, int]]:
    """(problem-id, hops-so-far) for every open proof still under the hop cap.

    pass-1 is one-shot: it gates a partial and stops. The proven route to a
    close is run.py's hop loop — re-dispatch with the PRIOR hop's boundary
    comment fed forward, up to 3 hops (Joe, 2026-08-06: "we had to redispatch
    Codex several times on a few"). Nothing routed statement-campaign partials
    into it, so 47 partials with documented boundaries were sitting idle.

    Selecting on `status == "partial"` alone (the first version) made the lane a
    no-op once those 55 hit the cap: it logged "no partials under the hop cap"
    every five minutes while 103 `skipped` and 51 `approved` artifacts — which
    have open sorries and have never had a closing attempt — stayed invisible.
    Eligibility is therefore "has a sorry and a settled statement", not "was
    once gated partial". Ordering matters because the budget is finite: hop-3
    partials closed 1 of 45, so they go last.
    """

    state = manifest_state()
    hops = closer_hops()
    priority = {pid: i for i, pid in enumerate(closer_priority())}
    awaiting = awaiting_review()
    out = []
    for lean_file in sorted(REPO.glob("problems/*/lean/Main.lean")):
        pid = lean_file.parts[-3]
        status = state.get(pid)
        if status not in CLOSER_ELIGIBLE or pid in awaiting:
            continue
        if hops.get(pid, 0) >= MAX_CLOSER_HOPS:
            continue
        sorries = gates.count_sorries(lean_file.read_text(encoding="utf-8"))
        if sorries == 0:
            continue
        out.append((priority.get(pid, len(priority)), _CLOSER_RANK[status],
                    sorries, pid, hops.get(pid, 0)))
    out.sort()
    return [(pid, h) for _pri, _rank, _n, pid, h in out]


def boundary_excerpt(problem_id: str) -> str:
    """The source just above the first sorry — where the boundary note lives."""

    lean_file = REPO / "problems" / problem_id / "lean" / "Main.lean"
    source = lean_file.read_text(encoding="utf-8")
    sites = gates.sorry_sites(source)
    if not sites:
        return "(no sorry site found)"
    line = sites[0]
    lines = source.splitlines()
    return "\n".join(lines[max(0, line - 13):line - 1]).strip() or "(no boundary note)"


ESCALATION = HERE / "escalation-queue.jsonl"

# Keeps the namespace, unlike gates.MATHLIB_IDENTIFIER_RE, whose word boundary
# starts matching AFTER the dot. Clustering is the point here, and a bare
# `tendsto_setIntegral` would merge `MeasureTheory`'s with anyone else's.
BLOCKER_RE = re.compile(
    r"`?\b([A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)+|"
    r"[A-Za-z][A-Za-z0-9']*_[A-Za-z0-9_']+)\b`?")
# Dotted-name matching also catches filenames and prose abbreviations, and they
# rank HIGH because every boundary note mentions the index — a ranked list whose
# top entry is `INDEX.md` is not a list anyone will act on.
BLOCKER_SUFFIX_STOP = frozenset({"md", "lean", "jsonl", "json", "txt", "py", "sh"})
BLOCKER_STOP = frozenset({"a.e", "i.e", "e.g", "etc", "w.r.t", "s.t", "resp"})


def awaiting_review() -> set[str]:
    """Problems whose statement has been re-formalized but not yet reviewed.

    `pending-review` is written BY A GATE, so a later gate can overwrite it and
    quietly restore closer-eligibility: on 2026-08-08 b97J01 went
    flagged -> pending-review (repair) -> partial (pass1-gate) inside ten
    minutes, and that last record would have handed an UNREVIEWED statement
    back to the closer. Proving against an unreviewed statement is the exact
    failure the bank exists to prevent, so eligibility has to look past the
    latest row: a problem is awaiting review whenever its newest
    `pending-review` is newer than its newest reviewer verdict.
    """

    pending: dict[str, str] = {}
    reviewed: dict[str, str] = {}
    if MANIFEST.exists():
        for line in MANIFEST.read_text(encoding="utf-8").splitlines():
            rec = json.loads(line)
            pid = rec["problem-id"]
            if rec.get("reviewer"):
                reviewed[pid] = rec["at"]
            elif rec.get("status") == "pending-review":
                pending[pid] = rec["at"]
    return {pid for pid, at in pending.items() if at > reviewed.get(pid, "")}


def closer_hops() -> dict[str, int]:
    """Hops ATTEMPTED per problem — annulled ones do not count against the cap.

    A hop whose job died before reaching a seat (quota exhaustion, a downed
    seat) bought nothing, so charging it to the 3-hop budget would retire a
    problem that was never actually tried. `closer-hop-annulled` records
    cancel their hop.
    """

    hops: dict[str, int] = {}
    if LEDGER.exists():
        for line in LEDGER.read_text(encoding="utf-8").splitlines():
            rec = json.loads(line)
            transition = rec.get("transition")
            if transition not in ("closer-hop", "closer-hop-annulled"):
                continue
            pid = (rec.get("problems") or [rec.get("problem-id")])[0]
            hops[pid] = hops.get(pid, 0) + (1 if transition == "closer-hop" else -1)
    return {p: n for p, n in hops.items() if n > 0}


def escalation_candidates() -> list[tuple[str, int, str]]:
    """(problem-id, hops, boundary) for problems the hop loop has given up on.

    A problem that spends MAX_CLOSER_HOPS without closing simply DISAPPEARS from
    `closer_targets` — which is how the lane quietly became a no-op earlier today
    (55 exhausted partials, and the log said only "no partials under the hop
    cap"). Silent exhaustion is the failure mode; this makes the survivors a
    queue instead.

    They are the best-documented problems in the corpus, not the worst: three
    independent agents each left an enriched boundary note saying exactly where
    the bridge stayed open. Measured yield says to stop paying for hop 4 —
    hop 1 closes 7%, hop 2 13.6%, hop 3 5.4% — so what these need is not another
    dispatch but shared machinery (Joe, 2026-08-07: "the problem might need
    further scrutiny, e.g. a ConstructionTarget").
    """

    state = manifest_state()
    hops = closer_hops()
    out = []
    for lean_file in sorted(REPO.glob("problems/*/lean/Main.lean")):
        pid = lean_file.parts[-3]
        if hops.get(pid, 0) < MAX_CLOSER_HOPS:
            continue
        if state.get(pid) in ("flagged", "void-statement-changed"):
            continue
        if gates.count_sorries(lean_file.read_text(encoding="utf-8")) == 0:
            continue
        out.append((pid, hops[pid], boundary_excerpt(pid)))
    return out


def escalation_clusters(candidates: list[tuple[str, int, str]],
                        min_share: int = 2) -> list[tuple[str, list[str]]]:
    """Mathlib-ish identifiers named by MORE THAN ONE stuck boundary note.

    This is the whole point of collecting the exhausted set rather than just
    listing it. One problem blocked on a missing lemma is a problem; five
    blocked on the SAME lemma is a ConstructionTarget with its cost already
    amortised. Ranking by how many distinct problems cite an identifier turns
    "what should we build next" into a sort rather than a judgement call.
    """

    where: dict[str, set[str]] = {}
    for pid, _hops, boundary in candidates:
        for ident in set(BLOCKER_RE.findall(boundary or "")):
            ident = ident.strip("`")
            if "_" not in ident and "." not in ident:
                continue
            if ident.lower() in BLOCKER_STOP:
                continue
            if ident.rsplit(".", 1)[-1].lower() in BLOCKER_SUFFIX_STOP:
                continue
            where.setdefault(ident, set()).add(pid)
    shared = [(i, sorted(p)) for i, p in where.items() if len(p) >= min_share]
    shared.sort(key=lambda kv: (-len(kv[1]), kv[0]))
    return shared


def cmd_escalate(args) -> int:
    """Record hop-exhausted problems and rank shared blockers for promotion."""

    candidates = escalation_candidates()
    if not candidates:
        print("no hop-exhausted problems")
        return 0
    clusters = escalation_clusters(candidates, min_share=args.min_share)
    if not args.dry_run:
        stamp = now_iso()
        for pid, hops, boundary in candidates:
            append_jsonl(ESCALATION, {
                "at": stamp, "problem-id": pid, "hops": hops,
                "status": "construction-target-candidate",
                "boundary": (boundary or "")[:1200]})
    print("hop-exhausted problems: %d" % len(candidates))
    for pid, hops, _b in candidates:
        print("   %-9s %d hops" % (pid, hops))
    print("\nblockers cited by %d+ of them (build these first):" % args.min_share)
    if not clusters:
        print("   (none shared — no promotion is amortised yet)")
    for ident, pids in clusters[: args.limit or 25]:
        print("   %-44s %d  %s" % (ident[:44], len(pids), " ".join(pids[:8])))
    if args.dry_run:
        print("\n(dry run — nothing written)")
    else:
        print("\nqueue: %s" % ESCALATION)
    return 0


def closer_framing(problem_id: str, hops: int) -> str:
    """Tell the closer truthfully what came before it.

    The template used to assert flatly that "the boundary comment at the sorry
    site records the prior hop's exact state". For the 189 targets the widened
    eligibility exposes that is FALSE — they are at hop 1, and the ones that
    were `skipped` have no boundary note at all, because the agent that skipped
    them never wrote one. Shipping a packet whose premise the recipient can see
    is wrong is how a spec error survives to the artifact; say which case it is.
    """

    if hops > 0:
        return ("The boundary comment at the sorry site records the prior "
                f"hop's exact state (this is hop {hops + 1}; {hops} before it "
                "left the bridge open). Go BEYOND what it already tried.")
    if boundary_excerpt(problem_id) == "(no boundary note)":
        return ("FIRST attempt at this artifact — there is no prior hop and no "
                "boundary note. Nothing has been ruled out for you; treat the "
                "search as wide open.")
    return ("FIRST closer attempt at this artifact. Any comment below is from "
            "the original formalization, NOT from a previous closing hop, so "
            "do not read it as a record of what has already been tried.")


def cmd_closer(args) -> int:
    """Run closer hops over partials, feeding each hop the prior boundary."""

    import subprocess
    import threading
    import time

    import render

    targets = closer_targets()
    if args.problems:
        wanted = {p for p in args.problems.split(",") if p}
        targets = [t for t in targets if t[0] in wanted]
    if args.limit:
        targets = targets[: args.limit]
    if not targets:
        print("no partials under the hop cap")
        return 0
    if args.dry_run:
        for pid, hops in targets:
            print(f"{pid}: hop {hops + 1} of {MAX_CLOSER_HOPS}")
        print(f"({len(targets)} targets)")
        return 0

    seats = args.seats.split(",")
    lock = threading.Lock()
    outcomes: list[tuple[str, str]] = []
    queue = list(enumerate(targets))
    nonlocal_failures = [0]   # consecutive dispatch failures, shared under `lock`

    def work(i: int, pid: str, hops: int) -> None:
        # Re-check eligibility at dispatch time, not just when the queue was
        # built. A single lane invocation walks 186 targets over several hours,
        # so a statement flagged DURING the run stayed on its worklist: t00J04
        # was flagged at 19:04 and a hop was spent proving it at 20:56, closing
        # a statement already condemned for a missing part. A manifest read per
        # hop is nothing next to a Codex dispatch.
        current = manifest_state().get(pid)
        if current not in CLOSER_ELIGIBLE:
            print(f"{pid}: dropped, no longer eligible ({current})", flush=True)
            return
        lean_rel = f"problems/{pid}/lean/Main.lean"
        source = (REPO / lean_rel).read_text(encoding="utf-8")
        sha = subprocess.run(["git", "log", "-1", "--pretty=%H", "--", lean_rel],
                             cwd=REPO, capture_output=True, text=True).stdout.strip()
        packet = render.render("closer", {
            "hop_n": str(hops + 1),
            "problem_id": pid,
            "main_lean_path": lean_rel,
            "base_commit": sha or "unreported",
            "sorry_count": str(gates.count_sorries(source)),
            "boundary_framing": closer_framing(pid, hops),
            "boundary_excerpt": boundary_excerpt(pid),
            "statement_hash": str(frozen_hash(pid)),
        })
        seat = seats[i % len(seats)]
        # Dispatch FIRST, then log the hop. Logging first spent a hop on a
        # dispatch that never happened, so an exhausted-quota window would
        # silently burn all three hops of every remaining target without a
        # single attempt ever reaching a seat.
        try:
            job = agency.dispatch_fn(seat, packet)["job-id"]
        except agency.AgencyError as exc:
            print(f"{pid}: dispatch failed ({exc})", flush=True)
            with lock:
                nonlocal_failures[0] += 1
                dead = nonlocal_failures[0] >= DISPATCH_FAILURE_LIMIT
            if dead:
                with lock:
                    queue.clear()
                print(f"stopping: {DISPATCH_FAILURE_LIMIT} consecutive dispatch "
                      "failures (quota exhausted or seats down)", flush=True)
            return
        with lock:
            nonlocal_failures[0] = 0
        append_jsonl(LEDGER, {"at": now_iso(), "transition": "closer-hop",
                              "problem-id": pid, "problems": [pid],
                              "hop": hops + 1, "seat": seat})
        # Wait on the ARTIFACT as well as the job record. A moved commit sha is
        # proof the hop is over, and it is observable immediately rather than
        # on the next 60s poll tick — so a hop that commits releases its seat
        # sooner. It also means a job that fails to finalize (for whatever
        # reason) costs one hop rather than the full JOB_TIMEOUT_SECONDS, with
        # the timeout left as a genuine backstop instead of the primary path.
        deadline = time.time() + JOB_TIMEOUT_SECONDS
        broke_on, job_state = "timeout", ""
        while time.time() < deadline:
            job_state = str(agency.poll_fn(job).get("status") or "")
            if job_state in ("done", "failed", "error"):
                broke_on = "job"
                break
            now_sha = subprocess.run(
                ["git", "log", "-1", "--pretty=%H", "--", lean_rel],
                cwd=REPO, capture_output=True, text=True).stdout.strip()
            if now_sha and now_sha != sha:
                broke_on = "artifact"
                break
            time.sleep(POLL_SECONDS)
        if broke_on == "timeout":
            print(f"{pid}: job {job} hit the {JOB_TIMEOUT_SECONDS}s backstop "
                  "with no commit and no terminal state", flush=True)
        # A job that FAILED is not an attempt. The dispatch-failure breaker only
        # watched for AgencyError at POST time, but when the Codex window ran out
        # on 2026-08-08 dispatch kept succeeding and the job died seconds later
        # with "You've hit your usage limit" — so 55 hops were spent gating
        # unchanged artifacts as `partial`, 17 of them a problem's LAST hop.
        # Annul the hop and count it toward the breaker: the ledger should record
        # attempts, not POSTs.
        if job_state in ("failed", "error"):
            append_jsonl(LEDGER, {"at": now_iso(), "transition": "closer-hop-annulled",
                                  "problem-id": pid, "problems": [pid],
                                  "hop": hops + 1, "seat": seat,
                                  "reason": f"job-{job_state}"})
            with lock:
                nonlocal_failures[0] += 1
                dead = nonlocal_failures[0] >= DISPATCH_FAILURE_LIMIT
                if dead:
                    queue.clear()
            print(f"{pid}: job {job_state}, hop annulled", flush=True)
            if dead:
                print(f"stopping: {DISPATCH_FAILURE_LIMIT} consecutive job "
                      "failures (quota exhausted or seats down)", flush=True)
            return
        res = pass1_gate(pid)
        append_jsonl(MANIFEST, {"at": now_iso(), "problem-id": pid,
                                "status": res["outcome"],
                                "observed-statement-hash": res.get("statement-hash"),
                                "gate": f"closer-hop-{hops + 1}"})
        with lock:
            outcomes.append((pid, res["outcome"]))
        print(f"{pid} hop {hops + 1}: {res['outcome']}", flush=True)

    def worker() -> None:
        while True:
            with lock:
                if not queue:
                    return
                i, (pid, hops) = queue.pop(0)
            work(i, pid, hops)

    threads = [threading.Thread(target=worker)
               for _ in range(min(args.concurrency, len(targets)))]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    from collections import Counter
    print(dict(Counter(o for _, o in outcomes)))
    return 0


def cmd_integrity(args) -> int:
    """Re-hash every banked artifact against its frozen contract.

    The pass-1 gate only sees artifacts it dispatched. Statements also enter the
    bank by other paths — merges, manual conflict resolutions, recovered
    working-tree files — and those paths meet no gate at all. a97J07 was
    substituted on 2026-08-05 and entered as a legitimate close via a merge
    rule; nothing noticed for a day. This checks BANK STATE, not run output, so
    it is indifferent to how an artifact arrived.
    """

    state = manifest_state()
    in_flux = {p for p, _ in repair_targets()}
    drift, missing, unparsed, ok = [], [], [], 0
    for pid in sorted(state):
        expected = frozen_hash(pid)
        if not expected:
            continue
        lean_file = REPO / "problems" / pid / "lean" / "Main.lean"
        if not lean_file.exists():
            missing.append(pid)
            continue
        try:
            _n, _x, digest = gates.statement_hash(lean_file.read_text(encoding="utf-8"), pid)
        except gates.GateError as exc:
            unparsed.append((pid, str(exc)[:60]))
            continue
        if digest == expected:
            ok += 1
        elif pid in in_flux:
            pass  # a rejected statement under repair is meant to change
        else:
            drift.append((pid, state.get(pid, "?")))
    print(f"banked artifacts checked : {ok + len(drift) + len(unparsed) + len(missing)}")
    print(f"  contract intact        : {ok}")
    print(f"  under repair (expected): {len(in_flux)}")
    print(f"  SUBSTITUTED            : {len(drift)}")
    for pid, st in drift:
        print(f"      {pid}  (status={st[:60]})")
    if unparsed:
        print(f"  unparseable            : {len(unparsed)}")
        for pid, why in unparsed[:10]:
            print(f"      {pid}  {why}")
    if missing:
        print(f"  artifact missing       : {len(missing)}  {missing[:10]}")
    return 1 if drift else 0


def cmd_review(args) -> int:
    state = manifest_state()
    if args.problem not in state:
        print(f"{args.problem} not in manifest", file=sys.stderr)
        return 1
    record = {"at": now_iso(), "problem-id": args.problem,
              # Bare "flagged" with the reason in `note`. Packing the reason
              # INTO the status makes `manifest_state` report an essay as a
              # status, and every status-keyed tally then misses the flag.
              "status": "approved" if args.approve else "flagged",
              "reviewer": args.reviewer}
    if not args.approve:
        record["note"] = args.reason
    if args.approve:
        # Re-freeze the claim set to what was actually reviewed. A repair may
        # legitimately replace a file's declarations — a01J04 and a97A04 both
        # gained a real `apm_<pid>` theorem where the file had carried only
        # ad-hoc helpers — and approving that without re-freezing leaves the
        # contract naming the OLD claims. `declaration_set_drift` then reports
        # them REMOVED and voids every later hop: a01J04 was closed at hop 1
        # and the close was discarded as `void-statement-changed`.
        lean_file = REPO / "problems" / args.problem / "lean" / "Main.lean"
        if lean_file.exists():
            source = lean_file.read_text(encoding="utf-8")
            record["declaration-hashes"] = gates.declaration_hashes(source)
    append_jsonl(MANIFEST, record)
    print(f"{args.problem}: {'approved' if args.approve else 'flagged'}")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    sub = parser.add_subparsers(dest="cmd", required=True)
    runp = sub.add_parser("run")
    runp.add_argument("--n", type=int, default=5)
    runp.add_argument("--batch-size", type=int, default=3)
    runp.add_argument("--seats", default="codex-13,codex-14")
    runp.add_argument("--problems", default="", help="comma-separated; overrides candidate selection")
    statusp = sub.add_parser("status")  # noqa: F841
    passp = sub.add_parser("pass1")
    passp.add_argument("--seats", default="zai-1")
    passp.add_argument("--set-size", type=int, default=10)
    # The Z.ai quota yields ~10 problems per 5-hour window; dispatching the
    # whole approved bank just bounces the tail with HTTP 429 and loses the
    # bookkeeping. --limit sizes a dispatch to the window; --retry-attempted
    # re-offers problems that already produced a pass-1 commit.
    passp.add_argument("--limit", type=int, default=0, help="0 = no limit")
    passp.add_argument("--retry-attempted", action="store_true")
    passp.add_argument("--dry-run", action="store_true")
    # The zai lane's packet leans on memory_search/psr_search, which is a
    # zai-adapter tool; the codex lane gets a sibling-precedent packet instead.
    passp.add_argument("--template", default="pass1-prove.md")
    closerp = sub.add_parser("closer")
    closerp.add_argument("--seats", default="ams-codex-1")
    closerp.add_argument("--problems", default="")
    closerp.add_argument("--limit", type=int, default=0)
    closerp.add_argument("--concurrency", type=int, default=1)
    closerp.add_argument("--dry-run", action="store_true")
    sub.add_parser("integrity")
    repairp = sub.add_parser("repair")
    repairp.add_argument("--seats", default="ams-codex-1")
    repairp.add_argument("--problems", default="", help="comma-separated; default = all rejected")
    repairp.add_argument("--limit", type=int, default=0)
    repairp.add_argument("--concurrency", type=int, default=2)
    repairp.add_argument("--dry-run", action="store_true")
    gatep = sub.add_parser("pass1-gate")
    gatep.add_argument("--regate", action="store_true",
                       help="re-run the build even for unchanged artifacts")
    gatep.add_argument("--problems", default="", help="comma-separated; default = all ungated")
    drainp = sub.add_parser("pass1-drain")
    drainp.add_argument("--seats", default="zai-1,zai-2,zai-3,zai-4")
    drainp.add_argument("--tranche", type=int, default=10)
    drainp.add_argument("--template", default="pass1-prove.md")
    drainp.add_argument("--min-available", type=float, default=0.0)
    drainp.add_argument("--max-hours", type=float, default=168.0)
    drainp.add_argument("--tranche-timeout-hours", type=float, default=3.0)
    escp = sub.add_parser("escalate")
    escp.add_argument("--min-share", type=int, default=2)
    escp.add_argument("--limit", type=int, default=25)
    escp.add_argument("--dry-run", action="store_true")
    reviewp = sub.add_parser("review")
    reviewp.add_argument("problem")
    reviewp.add_argument("--approve", action="store_true")
    reviewp.add_argument("--reason", default="")
    reviewp.add_argument("--reviewer", default="claude-3")
    args = parser.parse_args()

    if args.cmd == "review":
        return cmd_review(args)
    if args.cmd == "escalate":
        return cmd_escalate(args)
    if args.cmd == "closer":
        return cmd_closer(args)
    if args.cmd == "integrity":
        return cmd_integrity(args)
    if args.cmd == "repair":
        return cmd_repair(args)
    if args.cmd == "pass1-gate":
        return cmd_pass1_gate(args)
    if args.cmd == "pass1-drain":
        return cmd_pass1_drain(args)
    if args.cmd == "pass1":
        state = manifest_state()
        approved = [p for p, st in sorted(state.items()) if st == "approved"]
        if not approved:
            print("no approved statements")
            return 1
        if not args.retry_attempted:
            attempted = pass1_attempted()
            skipped = [p for p in approved if p in attempted]
            approved = [p for p in approved if p not in attempted]
            if skipped:
                print(f"excluding {len(skipped)} already-attempted: {','.join(skipped)}")
        if args.limit:
            deferred = len(approved) - args.limit
            approved = approved[: args.limit]
            if deferred > 0:
                print(f"limit {args.limit}: deferring {deferred} to a later window")
        if args.dry_run:
            print(f"would dispatch {len(approved)}: {','.join(approved)}")
            return 0
        seats = args.seats.split(",")
        template = (HERE / "templates" / args.template).read_text(encoding="utf-8")
        stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%d-%H%M")
        # One job per problem: a multi-problem set overruns the job cap;
        # per-problem jobs queue per seat and each fits comfortably.
        for i, pid in enumerate(approved):
            seat = seats[i % len(seats)]
            packet = template.replace("{problem_list}", f"\n- `{pid}`")
            job = agency.dispatch_fn(seat, packet)["job-id"]
            append_jsonl(LEDGER, {"at": now_iso(), "batch": f"pass1-{stamp}-{i:03d}",
                                  "transition": "pass1-dispatch", "seat": seat,
                                  "problems": [pid], "job-id": job})
        from collections import Counter
        per_seat = Counter(seats[i % len(seats)] for i in range(len(approved)))
        print(f"dispatched {len(approved)} pass-1 jobs: {dict(per_seat)}")
        return 0
    if args.cmd == "status":
        state = manifest_state()
        from collections import Counter
        print(dict(Counter(state.values())), f"| candidates remaining: {len(candidate_problems())}")
        return 0

    state = manifest_state()
    # missing/defective are RETRYABLE (e.g. a network-killed batch);
    # only settled statuses exclude a problem from selection.
    settled = ("approved", "pending-review")
    if args.problems:
        # Targeted re-formalization. candidate_problems() only offers bundles
        # with NO artifact, so it cannot see a bundle whose Main.lean exists but
        # is empty scaffolding (24 such "clean" stubs found 2026-08-07).
        todo = [p for p in args.problems.split(",") if p]
    else:
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
