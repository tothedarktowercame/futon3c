import contextlib
import io
import tempfile
import unittest
from pathlib import Path

import driver
import run


def gate(outcome, digest="sha256:frozen"):
    return {
        "outcome": outcome,
        "statement-hash": digest,
        "gate-results": {
            "build": {"exit-code": 0},
            "sorries": 0 if outcome == "closed" else 1,
            "boundary-conforming": outcome == "partial",
            "boundary-sites": [{"line": 12}] if outcome == "partial" else [],
            "axioms": {"line": "axioms"},
        },
    }


class Harness:
    def __init__(self, root, gates):
        self.root = Path(root)
        self.gates = list(gates)
        self.dispatches = []
        self.polls = []
        self.capability = []
        self.promotions = []
        self.next_job = 0

    def dispatch(self, seat, packet):
        self.next_job += 1
        job = f"job-{self.next_job}"
        self.dispatches.append((seat, packet, job))
        return {"job-id": job, "request": {"from": "apm-driver", "to": seat, "body": packet}}

    def poll(self, job_id):
        self.polls.append(job_id)
        packet = next(packet for _seat, packet, job in self.dispatches if job == job_id)
        if packet.startswith("SCRIBE PASS"):
            return {
                "status": "done",
                "result": "Promoted memory id: e-memory-abcdef\nApprovals: /tmp/chain/APPROVALS.md",
            }
        return {"status": "done", "result": f"Commit SHA: abcdef{self.next_job}"}

    def gate(self, _problem):
        return self.gates.pop(0)

    def checkpoint_request(self, **kwargs):
        payload = {"checkpoint": kwargs["checkpoint"], "job-id": "review-job"}
        if kwargs["resume_state"]:
            payload["resume-state"] = kwargs["resume_state"]
        driver.append_transition(
            kwargs["ledger_path"],
            driver.make_record(kwargs["chain_id"], kwargs["problem_id"], "review-request", payload),
        )
        return {"job-id": "review-job"}

    def checkpoint_watch(self, **kwargs):
        state = driver.fold_ledger(driver.read_ledger(kwargs["ledger_path"]))[kwargs["chain_id"]]
        driver.append_transition(
            kwargs["ledger_path"],
            driver.make_record(state["chain-id"], state["problem-id"], "verdict", {"verdict": "approve"}),
        )
        return {"status": "applied"}

    def promotion_queue(self, **kwargs):
        self.promotions.append(kwargs)
        return kwargs

    def capability_update(self, _path, **kwargs):
        self.capability.append(kwargs)
        return {"updated": True, "bullet": kwargs["chain_id"]}

    def deps(self):
        return run.Dependencies(
            dispatch=self.dispatch,
            poll=self.poll,
            gate=self.gate,
            renderer=lambda name, params: run.render.render(name, params),
            quota=lambda: None,
            checkpoint_request=self.checkpoint_request,
            checkpoint_watch=self.checkpoint_watch,
            promotion_queue=self.promotion_queue,
            sleep=lambda _seconds: None,
            statement=lambda problem: f"theorem main_{problem} : True :=",
            boundary=lambda _problem, _gate: "-- searched Mathlib\n-- exact blocker",
            capability_update=self.capability_update,
        )

    def runner(self):
        return run.Runner(self.deps(), config={
            "ledger": self.root / "ledger.jsonl",
            "verdicts": self.root / "verdicts",
            "promotion_queue": self.root / "promotion.jsonl",
            "capability_proof": self.root / "capability.md",
            "apm_repo": self.root / "apm-lean",
            "poll_seconds": 0,
            "review_poll_seconds": 0,
        })


class CompositionTests(unittest.TestCase):
    def test_selection_resumes_active_chain_before_cron_queue(self):
        with tempfile.TemporaryDirectory() as directory:
            ledger = Path(directory) / "ledger.jsonl"
            driver.append_transition(
                ledger,
                driver.make_record("active", "a96J13", "select", {}),
            )
            self.assertEqual(
                "a96J13",
                run.select_problem(None, ledger_path=ledger, excluded=set()),
            )

    def test_full_happy_chain_once(self):
        with tempfile.TemporaryDirectory() as directory:
            harness = Harness(directory, [gate("closed")])
            state = harness.runner().run_chain("a96J09")
            self.assertEqual("DONE", state["state"])
            self.assertEqual("closed", state["outcome"])
            self.assertEqual(["zai-1", "zai-1", "codex-12"], [d[0] for d in harness.dispatches])
            phase_b = harness.dispatches[1][1]
            self.assertIn("PHASE A REPORT EXCERPT", phase_b)
            self.assertIn("Commit SHA: abcdef1", phase_b)
            self.assertEqual(1, len(harness.promotions))
            transitions = [r["transition"] for r in driver.read_ledger(harness.root / "ledger.jsonl")]
            self.assertEqual("chain-close", transitions[-1])

    def test_partial_closer_then_closed(self):
        with tempfile.TemporaryDirectory() as directory:
            harness = Harness(directory, [gate("partial"), gate("closed")])
            state = harness.runner().run_chain("a96J10")
            self.assertEqual("DONE", state["state"])
            self.assertEqual(1, state["hops"])
            closer = [packet for _seat, packet, _job in harness.dispatches if packet.startswith("CLOSER HOP")]
            self.assertEqual(1, len(closer))
            self.assertIn("sha256:frozen", closer[0])
            self.assertIn("searched Mathlib", closer[0])

    def test_defective_requests_anomaly_and_stops(self):
        with tempfile.TemporaryDirectory() as directory:
            harness = Harness(directory, [gate("defective")])
            state = harness.runner().run_chain("a96J11")
            self.assertEqual("AWAITING_REVIEW", state["state"])
            self.assertEqual("anomaly", state["review-checkpoint"])
            self.assertEqual(2, len(harness.dispatches))

    def test_dry_run_renders_all_and_dispatches_none(self):
        with tempfile.TemporaryDirectory() as directory:
            harness = Harness(directory, [])
            runner = harness.runner()
            output = io.StringIO()
            with contextlib.redirect_stdout(output):
                runner.dry_run("a96J12")
            rendered = output.getvalue()
            for name in ("phase-a", "phase-b", "closer", "scribe"):
                self.assertIn(f"===== {name} =====", rendered)
            self.assertEqual([], harness.dispatches)
            self.assertFalse((harness.root / "ledger.jsonl").exists())

    def test_resume_after_dispatch_b_polls_existing_job(self):
        with tempfile.TemporaryDirectory() as directory:
            harness = Harness(directory, [gate("closed")])
            runner = harness.runner()
            ledger = harness.root / "ledger.jsonl"
            chain = "resume-chain"
            records = [
                driver.make_record(chain, "a96J13", "select", {}),
                driver.make_record(chain, "a96J13", "dispatch-a", {"job-id": "job-a"}),
                driver.make_record(chain, "a96J13", "poll", {"job-id": "job-a", "status": "done", "result": "A report"}),
                driver.make_record(chain, "a96J13", "dispatch-b", {"job-id": "job-b"}),
            ]
            for record in records:
                driver.append_transition(ledger, record)
            harness.dispatches.extend([
                ("zai-1", "phase-a already sent", "job-a"),
                ("zai-1", "phase-b already sent", "job-b"),
            ])
            state = runner.run_chain("a96J13")
            self.assertEqual("DONE", state["state"])
            self.assertEqual("job-b", harness.polls[0])
            self.assertNotIn("PHASE A ONLY", [packet for _seat, packet, _job in harness.dispatches[2:]])


class CapabilityUpdateTests(unittest.TestCase):
    def test_append_is_idempotent_and_preserves_other_sections(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "capability.md"
            path.write_text("# Proof\n\n## Update log\n\n- old\n\n## Appendix\n\nkeep\n", encoding="utf-8")
            first = run.append_capability_update(
                path, chain_id="chain-1", problem_id="a96J09", outcome="closed",
                hops=1, shas=["abc1234"], today=run.dt.date(2026, 8, 4),
            )
            once = path.read_text(encoding="utf-8")
            second = run.append_capability_update(
                path, chain_id="chain-1", problem_id="a96J09", outcome="closed",
                hops=1, shas=["abc1234"], today=run.dt.date(2026, 8, 4),
            )
            self.assertTrue(first["updated"])
            self.assertEqual("already-recorded", second["reason"])
            self.assertEqual(once, path.read_text(encoding="utf-8"))
            self.assertTrue(path.read_text(encoding="utf-8").endswith("## Appendix\n\nkeep\n"))

    def test_missing_marker_refuses_without_write(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "capability.md"
            path.write_text("# no update section\n", encoding="utf-8")
            before = path.read_bytes()
            result = run.append_capability_update(
                path, chain_id="chain-2", problem_id="a96J10", outcome="open-hole",
                hops=3, shas=[],
            )
            self.assertFalse(result["updated"])
            self.assertEqual(before, path.read_bytes())


if __name__ == "__main__":
    unittest.main()


class ResumeDoesNotStartFreshTest(unittest.TestCase):
    """2026-08-04 night fix: bare --once resume never begins new work."""

    def test_no_pending_chain_and_no_new_flag_raises(self):
        with self.assertRaisesRegex(run.RunError, "no pending chain"):
            run.select_problem(None, ledger_path=None, allow_new=False)
