import datetime as dt
import json
import tempfile
import unittest
from pathlib import Path

import checkpoints
import driver
from driver_test import BASE, Fixture


STATEMENT = """theorem apm_demo (n : Nat) :
    n = n := by"""
INFORMAL = [
    "/home/joe/code/apm-lean/problems/a00J01/problem.md",
    "/home/joe/code/apm-lean/problems/a00J01/informal-solution.md",
    "/home/joe/code/apm-lean/problems/a00J01/problem.tex",
]
GATES = {"build": {"exit-code": 0}, "sorries": 0, "axioms": {"line": "clean"}}


class Harness:
    def __init__(self, directory, outcome="closed"):
        self.root = Path(directory)
        self.ledger = self.root / "ledger.jsonl"
        self.verdicts = self.root / "verdicts"
        self.fixture = Fixture(chain="chain-check", problem="a00J01").starter(outcome)
        for record in self.fixture.records:
            driver.append_transition(self.ledger, record)
        self.dispatched = []

    def dispatch(self, target, body):
        request = {
            "from": "apm-driver",
            "to": target,
            "body": body,
            "mode": "work",
        }
        self.dispatched.append(request)
        return {"job-id": "checkpoint-job", "request": request}

    def request(self, checkpoint, *, resume_state=None, at=None):
        return checkpoints.request_checkpoint(
            chain_id="chain-check",
            problem_id="a00J01",
            checkpoint=checkpoint,
            lean_statement=STATEMENT,
            informal_paths=INFORMAL,
            gate_results=GATES,
            ledger_path=self.ledger,
            verdicts_dir=self.verdicts,
            dispatch=self.dispatch,
            resume_state=resume_state,
            now=at or BASE + dt.timedelta(minutes=10),
        )

    def write_json(self, checkpoint, verdict):
        self.verdicts.mkdir(parents=True, exist_ok=True)
        path = checkpoints.verdict_path(
            "chain-check", checkpoint, verdicts_dir=self.verdicts
        )
        path.write_text(
            json.dumps(
                {
                    "verdict": verdict,
                    "reviewer": "claude-10",
                    "at": "2026-08-04T08:11:00Z",
                    "notes": "reviewed against source",
                }
            ),
            encoding="utf-8",
        )
        return path


class RequestTests(unittest.TestCase):
    def test_fidelity_bell_is_self_contained_and_statement_is_verbatim(self):
        with tempfile.TemporaryDirectory() as directory:
            harness = Harness(directory)
            result = harness.request("fidelity")
            body = result["bell-body"]
            self.assertIn(STATEMENT, body)
            self.assertIn("Chain ID: chain-check", body)
            self.assertIn("Problem ID: a00J01", body)
            self.assertIn("Checkpoint kind: fidelity", body)
            for path in INFORMAL:
                self.assertIn(path, body)
            self.assertIn(json.dumps(GATES, indent=2, sort_keys=True), body)
            self.assertIn(result["verdict-file"], body)
            self.assertIn("{verdict, reviewer, at, notes}", body)
            self.assertEqual("claude-10", harness.dispatched[0]["to"])
            self.assertEqual("apm-driver", harness.dispatched[0]["from"])
            state = driver.fold_ledger(driver.read_ledger(harness.ledger))["chain-check"]
            self.assertEqual("AWAITING_REVIEW", state["state"])


class VerdictTests(unittest.TestCase):
    def test_fidelity_approve_and_reject_fold(self):
        for verdict, expected_state, expected_outcome in [
            ("approve", "CLOSED", "closed"),
            ("reject", "DONE", "fidelity-rejected"),
        ]:
            with self.subTest(verdict=verdict), tempfile.TemporaryDirectory() as directory:
                harness = Harness(directory)
                harness.request("fidelity")
                harness.write_json("fidelity", verdict)
                result = checkpoints.watch_verdict(
                    chain_id="chain-check",
                    checkpoint="fidelity",
                    ledger_path=harness.ledger,
                    verdicts_dir=harness.verdicts,
                    now=BASE + dt.timedelta(minutes=11),
                )
                self.assertEqual("applied", result["status"])
                self.assertEqual(expected_state, result["state"])
                self.assertEqual(expected_outcome, result["outcome"])
                self.assertEqual(
                    {
                        "verdict": verdict,
                        "reviewer": "claude-10",
                        "at": "2026-08-04T08:11:00Z",
                        "notes": "reviewed against source",
                    },
                    result["verdict"],
                )

    def test_anomaly_resume_and_abandon_fold_from_edn(self):
        for verdict, expected_state, expected_outcome in [
            ("resume", "GATE", None),
            ("abandon", "DONE", "abandoned"),
        ]:
            with self.subTest(verdict=verdict), tempfile.TemporaryDirectory() as directory:
                harness = Harness(directory, outcome="defective")
                harness.request("anomaly", resume_state="gate")
                harness.verdicts.mkdir(parents=True, exist_ok=True)
                path = checkpoints.verdict_path(
                    "chain-check", "anomaly", verdicts_dir=harness.verdicts
                )
                path.write_text(
                    "{:verdict :%s, :reviewer \"claude-10\", "
                    ":at \"2026-08-04T08:11:00Z\", :notes \"checked\"}\n"
                    % verdict,
                    encoding="utf-8",
                )
                result = checkpoints.watch_verdict(
                    chain_id="chain-check",
                    checkpoint="anomaly",
                    ledger_path=harness.ledger,
                    verdicts_dir=harness.verdicts,
                    now=BASE + dt.timedelta(minutes=11),
                )
                self.assertEqual(expected_state, result["state"])
                self.assertEqual(expected_outcome, result["outcome"])

    def test_malformed_verdict_is_error_and_not_ledgered(self):
        with tempfile.TemporaryDirectory() as directory:
            harness = Harness(directory)
            harness.request("fidelity")
            before = harness.ledger.read_bytes()
            harness.verdicts.mkdir(parents=True, exist_ok=True)
            path = checkpoints.verdict_path(
                "chain-check", "fidelity", verdicts_dir=harness.verdicts
            )
            path.write_text('{"verdict":"approve"}', encoding="utf-8")
            result = checkpoints.watch_verdict(
                chain_id="chain-check",
                checkpoint="fidelity",
                ledger_path=harness.ledger,
                verdicts_dir=harness.verdicts,
                now=BASE + dt.timedelta(minutes=11),
            )
            self.assertEqual("error", result["status"])
            self.assertIn("keys must be exactly", result["error"])
            self.assertEqual(before, harness.ledger.read_bytes())

    def test_stale_review_remains_parked_and_late_verdict_resumes(self):
        with tempfile.TemporaryDirectory() as directory:
            harness = Harness(directory, outcome="defective")
            requested = BASE + dt.timedelta(minutes=10)
            harness.request("anomaly", resume_state="gate", at=requested)
            late = requested + dt.timedelta(hours=13)
            waiting = checkpoints.watch_verdict(
                chain_id="chain-check",
                checkpoint="anomaly",
                ledger_path=harness.ledger,
                verdicts_dir=harness.verdicts,
                now=late,
            )
            self.assertEqual(
                {"status": "awaiting-review", "stale": True, "age-seconds": 46800,
                 "verdict-file": str(checkpoints.verdict_path(
                     "chain-check", "anomaly", verdicts_dir=harness.verdicts))},
                waiting,
            )
            chain = driver.fold_ledger(driver.read_ledger(harness.ledger))["chain-check"]
            rows = driver.active_status({"chain-check": chain}, now=late)
            self.assertTrue(rows[0]["review-stale"])
            self.assertEqual("STALE-REVIEW (13h 0m)", rows[0]["status"])
            self.assertIn("STALE-REVIEW (13h 0m)", driver.render_status(rows))

            harness.write_json("anomaly", "resume")
            resumed = checkpoints.watch_verdict(
                chain_id="chain-check",
                checkpoint="anomaly",
                ledger_path=harness.ledger,
                verdicts_dir=harness.verdicts,
                now=late + dt.timedelta(minutes=1),
            )
            self.assertEqual("GATE", resumed["state"])


class PromotionQueueTests(unittest.TestCase):
    def test_promotion_queue_is_append_only_and_does_not_touch_chain(self):
        with tempfile.TemporaryDirectory() as directory:
            queue = Path(directory) / "promotion-queue.jsonl"
            record = checkpoints.queue_promotion_approval(
                chain_id="chain-check",
                memory_ids=["e-memory-1", "e-memory-2"],
                approvals_file="/tmp/APPROVALS.md",
                queue_path=queue,
                now=BASE,
            )
            self.assertEqual(
                {
                    "chain-id": "chain-check",
                    "memory-ids": ["e-memory-1", "e-memory-2"],
                    "approvals-file": "/tmp/APPROVALS.md",
                    "queued-at": "2026-08-04T08:00:00Z",
                },
                record,
            )
            self.assertEqual(record, json.loads(queue.read_text(encoding="utf-8")))


if __name__ == "__main__":
    unittest.main()
