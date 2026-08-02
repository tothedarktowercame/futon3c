#!/usr/bin/env python3
"""Run one local, OS-isolated E2-pilot arm and emit its gate record.

Recall runs on Joe's side.  Only the assembled packet crosses the apmablate
boundary, as a direct Codex argument.  A receipt from any uid other than 1001,
or with any failed process-bound probe, is rejected before a run record exists.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Sequence

sys.path.insert(0, str(Path(__file__).resolve().parent))
import runner_gate  # noqa: E402


ROOT = Path(__file__).resolve().parent.parent
SOURCE_REPO = Path("/home/joe/code/apm-lean")
RUNS_ROOT = Path("/home/apmablate/runs")
EXPECTED_UID = 1001
LB_MEMORY = "e-codexpilot-analytic-order-at-least-two-implies-local-noninjectivity"
INC_MEMORY = "e-codexpilot-combine-Schwarz-rigidity-with-power-series-uniqueness"
ARM_WITHHOLD = {"control": None, "lb-ablated": LB_MEMORY, "inc-ablated": INC_MEMORY}
REQUIRED_TRUE = (
    "home-read-denied", "no-future-commits", "no-analysis-artifacts",
    "no-runner-side-store", "operator-store-unreadable", "own-store-clean",
    "source-repo-unreachable",
)
PACKET_MARKER = "=== ASSEMBLED PACKET (DRY RUN; NOT DISPATCHED) ===\n"
RECEIPT_MARKER = "\n=== OFFERED RECEIPT (DRY RUN; NOT WRITTEN) ===\n"


class PilotError(RuntimeError):
    """The apparatus refused a run before it became E2 data."""


@dataclass(frozen=True)
class RecallDelivery:
    packet: str
    receipt: dict[str, Any]


def canonical_json(value: Any) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(",", ":")).encode()


def parse_recall_dry_run(stdout: str) -> RecallDelivery:
    if PACKET_MARKER not in stdout or RECEIPT_MARKER not in stdout:
        raise PilotError("recall dry-run output is missing packet or offered-receipt marker")
    packet_tail = stdout.split(PACKET_MARKER, 1)[1]
    packet, receipt_text = packet_tail.split(RECEIPT_MARKER, 1)
    try:
        receipt = json.JSONDecoder().raw_decode(receipt_text.lstrip())[0]
    except (ValueError, TypeError) as error:
        raise PilotError(f"offered receipt is not JSON: {error}") from error
    return RecallDelivery(packet.rstrip(), receipt)


def receipt_body(receipt: dict[str, Any]) -> dict[str, Any]:
    return receipt.get("body", {})


def surfaced_ids(receipt: dict[str, Any]) -> list[str]:
    return list(receipt_body(receipt).get("memory-use", {}).get(
        "memory-use/surfaced-ids", []
    ))


def withheld_ids(receipt: dict[str, Any]) -> list[str]:
    return list(receipt_body(receipt).get("memory-use", {}).get(
        "memory-use/withheld-ids", []
    ))


def manipulation_check(arm: str, receipt: dict[str, Any]) -> dict[str, Any]:
    surfaced = set(surfaced_ids(receipt))
    withheld = set(withheld_ids(receipt))
    delivered = set(receipt_body(receipt).get("withholding-delivered-ids", []))
    target = ARM_WITHHOLD[arm]
    if arm == "control":
        passed = not withheld and {LB_MEMORY, INC_MEMORY}.issubset(surfaced)
        detail = "both registered memories surfaced and none was withheld"
    else:
        passed = target in withheld and target in delivered and target not in surfaced
        detail = "registered target was a candidate, was withheld, and did not surface"
    return {
        "passed": passed, "arm": arm, "target": target,
        "surfaced-ids": sorted(surfaced), "withheld-ids": sorted(withheld),
        "withholding-delivered-ids": sorted(delivered), "detail": detail,
    }


def validate_isolation_receipt(receipt: dict[str, Any]) -> dict[str, Any]:
    uid = receipt.get("effective-uid")
    if uid != EXPECTED_UID:
        raise PilotError(
            f"isolation receipt rejected: effective-uid {uid!r}, expected {EXPECTED_UID}"
        )
    failed = [key for key in REQUIRED_TRUE if receipt.get(key) is not True]
    if failed:
        raise PilotError("isolation receipt rejected: false/missing probes " + ", ".join(failed))
    probes = receipt.get("probes")
    if not isinstance(probes, list) or not probes or any(
        probe.get("passed") is not True for probe in probes
    ):
        raise PilotError("isolation receipt rejected: probe list is empty or contains a failure")
    actual = hashlib.sha256(canonical_json(probes)).hexdigest()
    if receipt.get("probe-result-hash") != actual:
        raise PilotError("isolation receipt rejected: probe-result-hash mismatch")
    return {
        "effective-uid": uid,
        "probe-result-hash": actual,
        "probe-count": len(probes),
        "valid": True,
    }


def attribution_check(report: str, offered: list[str]) -> dict[str, Any]:
    gate = runner_gate.UseAttributionGate()
    violations = gate.check(runner_gate.Run("e2-apmablate", "e2-local", report, offered))
    return {
        "passed": not violations,
        "violations": [violation.detail for violation in violations],
    }


def run_command(args: Sequence[str], **kwargs: Any) -> subprocess.CompletedProcess[str]:
    return subprocess.run(list(args), text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT, check=False, **kwargs)


def reset_staged_tree(problem: str, base: str) -> None:
    if not re.fullmatch(r"[A-Za-z0-9.-]+", problem):
        raise PilotError(f"unsafe problem id: {problem!r}")
    run_dir = RUNS_ROOT / problem
    if run_dir.parent != RUNS_ROOT:
        raise PilotError(f"unsafe run directory: {run_dir}")
    with tempfile.NamedTemporaryFile(prefix="e2-baseline-", suffix=".tar") as archive:
        archived = run_command(
            ["git", "-C", str(SOURCE_REPO), "archive", base, "-o", archive.name]
        )
        if archived.returncode:
            raise PilotError("could not archive registered baseline: " + archived.stdout[-1000:])
        os.chmod(archive.name, 0o644)
        commands = (
            ["sudo", "-n", "-u", "apmablate", "mkdir", "-p", str(run_dir)],
            ["sudo", "-n", "-u", "apmablate", "find", str(run_dir),
             "-mindepth", "1", "-depth", "-delete"],
            ["sudo", "-n", "-u", "apmablate", "tar", "-xf", archive.name,
             "-C", str(run_dir)],
            ["sudo", "-n", "-u", "apmablate", "mkdir", "-p", str(run_dir / ".lake")],
            ["sudo", "-n", "-u", "apmablate", "ln", "-sfn",
             "/home/apmablate/packages", str(run_dir / ".lake" / "packages")],
        )
        for command in commands:
            result = run_command(command)
            if result.returncode:
                raise PilotError("staged-tree reset failed: " + result.stdout[-1000:])


def run_recall(problem: str, arm: str) -> RecallDelivery:
    command = ["clojure", "-M", "scripts/dispatch_with_recall.clj",
               "--problem", problem, "--to", "e2-dry-run", "--dry-run"]
    target = ARM_WITHHOLD[arm]
    if target:
        command.extend(["--withhold", target])
    packet = f"""E2 PILOT ISOLATED PROOF RUN

Problem: {problem}
Registered base: 51b6bc00
Arm: {arm}
Target file: problems/a95J01/lean/Main.lean

Work only in the isolated staged repository supplied as your current directory.
Read its AGENTS.md and the problem's problem.md and informal-solution.md. Prove
the remaining `subordination_deriv_le` target without weakening or changing its
statement. Run `lake env lean problems/a95J01/lean/Main.lean`; completed targets
must be axiom-clean. Make one incremental git commit for every substantive proof
attempt, including an honest compiling partial if the target remains blocked.
Never inspect or request material outside this staged tree.

In the final report give exactly one `USED <id>: <mechanism>` or
`IGNORED <id>: <reason>` line for every surfaced memory id.
"""
    result = run_command(command, cwd=ROOT, input=packet)
    if result.returncode:
        raise PilotError("recall dry-run failed: " + result.stdout[-2000:])
    return parse_recall_dry_run(result.stdout)


def invoke_wrapper(problem: str, base: str, packet: str,
                   isolation_path: Path) -> subprocess.CompletedProcess[str]:
    codex = "/home/apmablate/.npm-global/bin/codex"
    command = [str(ROOT / "scripts/e2_ablation_dispatch.sh"),
               "--problem", problem, "--base-revision", base,
               "--receipt", str(isolation_path), "--", codex, "exec",
               "--ephemeral", "--ignore-user-config", "--ignore-rules",
               "--cd", str(RUNS_ROOT / problem), packet]
    return run_command(command, cwd=ROOT)


def extract_trace_twice(run_dir: Path, base: str,
                        extractor: Path | None = None) -> tuple[dict[str, Any], bool]:
    extractor = extractor or ROOT / "scripts/e2_decision_trace.py"
    outputs: list[bytes] = []
    for _ in range(2):
        result = run_command([sys.executable, str(extractor), str(run_dir), "--base", base])
        if result.returncode:
            raise PilotError("decision-trace extraction failed: " + result.stdout[-2000:])
        outputs.append(result.stdout.encode())
    try:
        trace = json.loads(outputs[0])
    except ValueError as error:
        raise PilotError(f"decision trace is not JSON: {error}") from error
    return trace, outputs[0] == outputs[1]


def build_record(arm: str, offered: dict[str, Any], isolation: dict[str, Any],
                 report: str, trace: dict[str, Any], stable: bool) -> dict[str, Any]:
    isolation_result = validate_isolation_receipt(isolation)
    manipulation = manipulation_check(arm, offered)
    attribution = attribution_check(report, surfaced_ids(offered))
    return {
        "schema": "e2-pilot-run/v1", "arm": arm,
        "offered-receipt": offered,
        "isolation-receipt": isolation_result,
        "trace-hash": trace.get("sha256"), "trace": trace,
        "extractor-stable": stable,
        "attribution": attribution, "manipulation": manipulation,
        "accepted": bool(stable and trace.get("attempt-count", 0) >= 1
                         and attribution["passed"] and manipulation["passed"]),
    }


def run_one(problem: str, base: str, arm: str, output: Path,
            *, recall_fn: Callable[[str, str], RecallDelivery] = run_recall,
            wrapper_fn: Callable[[str, str, str, Path], subprocess.CompletedProcess[str]] = invoke_wrapper,
            reset_fn: Callable[[str, str], None] = reset_staged_tree,
            extractor: Path | None = None) -> dict[str, Any]:
    delivery = recall_fn(problem, arm)
    reset_fn(problem, base)
    with tempfile.TemporaryDirectory(prefix="e2-pilot-") as temporary:
        isolation_path = Path(temporary) / "isolation.json"
        wrapped = wrapper_fn(problem, base, delivery.packet, isolation_path)
        if not isolation_path.is_file():
            raise PilotError(
                f"wrapper produced no isolation receipt (exit {wrapped.returncode}): "
                + wrapped.stdout[-1500:]
            )
        isolation = json.loads(isolation_path.read_text(encoding="utf-8"))
        # This check intentionally precedes trace extraction and output creation.
        validate_isolation_receipt(isolation)
        trace, stable = extract_trace_twice(RUNS_ROOT / problem, base, extractor)
        record = build_record(arm, delivery.receipt, isolation, wrapped.stdout, trace, stable)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_bytes(canonical_json(record) + b"\n")
    return record


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--problem", choices=["a95J01"], required=True)
    result.add_argument("--base", choices=["51b6bc00"], required=True)
    result.add_argument("--arm", choices=sorted(ARM_WITHHOLD), required=True)
    result.add_argument("--withhold", action="append", default=[])
    result.add_argument("--output", type=Path, required=True)
    return result


def main(argv: Sequence[str] | None = None) -> int:
    args = parser().parse_args(argv)
    expected = ARM_WITHHOLD[args.arm]
    requested = args.withhold or ([expected] if expected else [])
    if requested != ([expected] if expected else []):
        print("e2-pilot: --withhold does not match the registered arm", file=sys.stderr)
        return 2
    try:
        record = run_one(args.problem, args.base, args.arm, args.output)
    except PilotError as error:
        print(f"e2-pilot: {error}", file=sys.stderr)
        return 2
    print(json.dumps(record, sort_keys=True))
    return 0 if record["accepted"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
