import datetime as dt
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import driver


BASE = dt.datetime(2026, 8, 4, 8, 0, tzinfo=dt.timezone.utc)


class Fixture:
    def __init__(self, chain="chain-rich", problem="a96J09"):
        self.chain = chain
        self.problem = problem
        self.records = []
        self.tick = 0

    def add(self, transition, payload=None):
        at = (BASE + dt.timedelta(minutes=self.tick)).isoformat().replace("+00:00", "Z")
        self.tick += 1
        self.records.append(
            driver.make_record(self.chain, self.problem, transition, payload, at=at)
        )
        return self

    def starter(self, outcome="closed", statement_hash="sha256:statement"):
        return (
            self.add("select")
            .add("dispatch-a", {"job-id": "job-a"})
            .add("poll", {"job-id": "job-a", "status": "done"})
            .add("dispatch-b", {"job-id": "job-b"})
            .add("poll", {"job-id": "job-b", "status": "done"})
            .add(
                "gate",
                {
                    "outcome": outcome,
                    "statement-hash": statement_hash,
                    "gate-results": {
                        "lean-exit": 0,
                        "sorries": 0 if outcome == "closed" else 1,
                        "boundary-conforming": outcome != "defective",
                    },
                },
            )
        )

    def hop(self, number, outcome, statement_hash="sha256:statement"):
        job = f"job-closer-{number}"
        return (
            self.add(
                "closer-hop",
                {"job-id": job, "hop": number, "statement-hash": "sha256:statement"},
            )
            .add("poll", {"job-id": job, "status": "done"})
            .add(
                "gate",
                {
                    "outcome": outcome,
                    "statement-hash": statement_hash,
                    "gate-results": {
                        "lean-exit": 0,
                        "sorries": 0 if outcome == "closed" else 1,
                        "boundary-conforming": outcome != "defective",
                    },
                },
            )
        )

    def finish_closed(self):
        return (
            self.add("review-request", {"checkpoint": "fidelity"})
            .add("verdict", {"verdict": "approve"})
            .add("scribe", {"job-id": "job-scribe"})
            .add("poll", {"job-id": "job-scribe", "status": "done"})
            .add("promotion-queued", {"memory-ids": ["memory-1"]})
            .add("capability-update", {"sha": "capability-sha"})
            .add("chain-close", {"outcome": "closed"})
        )


class StateMachineTests(unittest.TestCase):
    def test_happy_close(self):
        fixture = Fixture().starter().finish_closed()
        state = driver.fold_ledger(fixture.records)[fixture.chain]
        self.assertEqual("DONE", state["state"])
        self.assertEqual("closed", state["outcome"])
        self.assertTrue(state["fidelity-approved"])
        self.assertEqual(0, state["hops"])

    def test_three_hop_closure(self):
        fixture = Fixture().starter("partial")
        fixture.hop(1, "partial").hop(2, "partial").hop(3, "closed").finish_closed()
        state = driver.fold_ledger(fixture.records)[fixture.chain]
        self.assertEqual("DONE", state["state"])
        self.assertEqual(3, state["hops"])
        self.assertEqual("sha256:statement", state["statement-hash"])

    def test_hop_exhaustion_becomes_open_hole(self):
        fixture = Fixture().starter("partial")
        fixture.hop(1, "partial").hop(2, "partial").hop(3, "partial")
        state = driver.fold_ledger(fixture.records)[fixture.chain]
        self.assertEqual("OPEN_HOLE", state["state"])
        self.assertEqual("open-hole", state["outcome"])
        self.assertEqual(3, state["hops"])
        self.assertEqual("capability-update", state["waiting-on"])
        fixture.add("capability-update", {"sha": "open-hole-capability-sha"})
        fixture.add("chain-close", {"outcome": "open-hole"})
        closed = driver.fold_ledger(fixture.records)[fixture.chain]
        self.assertEqual("DONE", closed["state"])
        self.assertEqual("open-hole", closed["outcome"])

    def test_statement_mismatch_voids_chain(self):
        fixture = Fixture().starter("partial").hop(1, "closed", "sha256:altered")
        state = driver.fold_ledger(fixture.records)[fixture.chain]
        self.assertEqual("VOID", state["state"])
        self.assertEqual("statement-altered", state["outcome"])
        self.assertEqual("sha256:statement", state["statement-hash"])
        fixture.add("review-request", {"checkpoint": "anomaly", "resume-state": "gate"})
        awaiting = driver.fold_ledger(fixture.records)[fixture.chain]
        self.assertEqual("AWAITING_REVIEW", awaiting["state"])
        self.assertEqual("VOID", awaiting["review-origin"])

    def test_defective_awaits_review(self):
        fixture = Fixture().starter("defective")
        fixture.add("review-request", {"checkpoint": "anomaly", "resume-state": "gate"})
        state = driver.fold_ledger(fixture.records)[fixture.chain]
        self.assertEqual("AWAITING_REVIEW", state["state"])
        self.assertEqual("DEFECTIVE", state["review-origin"])
        self.assertEqual("review:anomaly", state["waiting-on"])

    def test_verdict_resumes_defective_chain(self):
        fixture = Fixture().starter("defective")
        fixture.add("review-request", {"checkpoint": "anomaly", "resume-state": "gate"})
        fixture.add("verdict", {"verdict": "resume", "resume-state": "gate"})
        state = driver.fold_ledger(fixture.records)[fixture.chain]
        self.assertEqual("GATE", state["state"])
        self.assertEqual("mechanical-gate", state["waiting-on"])
        fixture.add(
            "gate",
            {
                "outcome": "partial",
                "statement-hash": "sha256:statement",
                "gate-results": {
                    "lean-exit": 0,
                    "sorries": 1,
                    "boundary-conforming": True,
                },
            },
        )
        self.assertEqual("PARTIAL", driver.fold_ledger(fixture.records)[fixture.chain]["state"])

    def test_fidelity_reject_closes_as_banked_outcome(self):
        fixture = Fixture().starter("closed")
        fixture.add("review-request", {"checkpoint": "fidelity"})
        fixture.add("verdict", {"verdict": "reject"})
        state = driver.fold_ledger(fixture.records)[fixture.chain]
        self.assertEqual("DONE", state["state"])
        self.assertEqual("fidelity-rejected", state["outcome"])

    def test_anomaly_abandon_closes_as_banked_outcome(self):
        fixture = Fixture().starter("defective")
        fixture.add("review-request", {"checkpoint": "anomaly", "resume-state": "gate"})
        fixture.add("verdict", {"verdict": "abandon"})
        state = driver.fold_ledger(fixture.records)[fixture.chain]
        self.assertEqual("DONE", state["state"])
        self.assertEqual("abandoned", state["outcome"])

    def test_illegal_verdict_kinds_raise(self):
        fidelity = Fixture().starter("closed")
        fidelity.add("review-request", {"checkpoint": "fidelity"})
        fidelity.add("verdict", {"verdict": "resume", "resume-state": "gate"})
        with self.assertRaisesRegex(driver.LedgerError, "invalid for fidelity"):
            driver.fold_ledger(fidelity.records)

        anomaly = Fixture().starter("defective")
        anomaly.add("review-request", {"checkpoint": "anomaly", "resume-state": "gate"})
        anomaly.add("verdict", {"verdict": "approve"})
        with self.assertRaisesRegex(driver.LedgerError, "invalid for review origin"):
            driver.fold_ledger(anomaly.records)

    def test_partial_requires_conforming_boundary(self):
        fixture = Fixture().starter("closed")
        fixture.records[-1]["payload"].update(
            outcome="partial",
            **{"gate-results": {"lean-exit": 0, "sorries": 1, "boundary-conforming": False}},
        )
        with self.assertRaisesRegex(driver.LedgerError, "conforming boundary"):
            driver.fold_ledger(fixture.records)

    def test_illegal_transition_is_not_appended(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "ledger.jsonl"
            selected = Fixture().add("select").records[0]
            driver.append_transition(path, selected)
            before = path.read_bytes()
            illegal = driver.make_record(
                "chain-rich", "a96J09", "dispatch-b", {"job-id": "too-early"}
            )
            with self.assertRaises(driver.LedgerError):
                driver.append_transition(path, illegal)
            self.assertEqual(before, path.read_bytes())

    def test_poll_must_match_active_job(self):
        fixture = Fixture().add("select").add("dispatch-a", {"job-id": "job-a"})
        fixture.add("poll", {"job-id": "wrong-job", "status": "done"})
        with self.assertRaisesRegex(driver.LedgerError, "does not match"):
            driver.fold_ledger(fixture.records)


class StatusTests(unittest.TestCase):
    def richest_fixture(self):
        fixture = Fixture().starter("partial").hop(1, "partial").hop(2, "defective")
        fixture.add("review-request", {"checkpoint": "anomaly", "resume-state": "gate"})
        return fixture

    def test_status_rich_fixture(self):
        fixture = self.richest_fixture()
        chains = driver.fold_ledger(fixture.records)
        now = BASE + dt.timedelta(minutes=20)
        rows = driver.active_status(chains, now=now)
        self.assertEqual(
            [
                {
                    "chain-id": "chain-rich",
                    "problem-id": "a96J09",
                    "state": "AWAITING_REVIEW",
                    "status": "AWAITING_REVIEW",
                    "review-stale": False,
                    "hops": 2,
                    "waiting-on": "review:anomaly",
                    "age-seconds": 1200,
                    "state-age-seconds": 480,
                    "statement-hash": "sha256:statement",
                }
            ],
            rows,
        )
        rendered = driver.render_status(rows)
        self.assertIn("AWAITING_REVIEW", rendered)
        self.assertIn("review:anomaly", rendered)

    def test_status_cli_json(self):
        fixture = self.richest_fixture()
        with tempfile.TemporaryDirectory() as directory:
            ledger = Path(directory) / "ledger.jsonl"
            for record in fixture.records:
                driver.append_transition(ledger, record)
            result = subprocess.run(
                [sys.executable, "driver.py", "--ledger", str(ledger), "status", "--json"],
                cwd=Path(__file__).parent,
                check=True,
                capture_output=True,
                text=True,
            )
            rows = json.loads(result.stdout)
            self.assertEqual("AWAITING_REVIEW", rows[0]["state"])
            self.assertEqual(2, rows[0]["hops"])

    def test_terminal_chains_are_not_active(self):
        fixture = Fixture().starter().finish_closed()
        rows = driver.active_status(driver.fold_ledger(fixture.records), now=BASE)
        self.assertEqual([], rows)
        self.assertEqual("No active chains.", driver.render_status(rows))


class InterfaceTests(unittest.TestCase):
    def test_h2_h3_interfaces_are_injected_stubs(self):
        for function in (driver.dispatch_fn, driver.poll_fn, driver.gate_fn):
            with self.assertRaises(NotImplementedError):
                function()
            self.assertTrue(function.__doc__)


if __name__ == "__main__":
    unittest.main()
