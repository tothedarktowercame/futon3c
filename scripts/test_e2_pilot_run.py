#!/usr/bin/env python3
from __future__ import annotations

import hashlib
import importlib.util
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


SCRIPTS = Path(__file__).resolve().parent


def load(name: str, filename: str):
    spec = importlib.util.spec_from_file_location(name, SCRIPTS / filename)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


pilot = load("e2_pilot_run_tested", "e2_pilot_run.py")
e3 = load("e2_pilot_e3_tested", "e2_pilot_e3.py")


def valid_isolation(uid: int = pilot.EXPECTED_UID, runner_exit: int = 0) -> dict:
    probes = [{"name": key, "passed": True, "exit-code": 1, "command": key}
              for key in pilot.REQUIRED_TRUE]
    return {
        "effective-uid": uid,
        "runner-exit-code": runner_exit,
        **{key: True for key in pilot.REQUIRED_TRUE},
        "probes": probes,
        "probe-result-hash": hashlib.sha256(pilot.canonical_json(probes)).hexdigest(),
    }


def offered(surfaced: list[str], withheld: list[str] | None = None,
            delivered: list[str] | None = None) -> dict:
    return {
        "body": {
            "memory-use": {
                "memory-use/surfaced-ids": surfaced,
                "memory-use/withheld-ids": withheld or [],
            },
            "withholding-delivered-ids": delivered or [],
        }
    }


class E2PilotTest(unittest.TestCase):
    def test_parse_recall_and_manipulation_both_directions(self) -> None:
        receipt = offered([pilot.LB_MEMORY, pilot.INC_MEMORY])
        stdout = (pilot.PACKET_MARKER + "PACKET\n" + pilot.RECEIPT_MARKER
                  + json.dumps(receipt) + "\n")
        delivery = pilot.parse_recall_dry_run(stdout)
        self.assertEqual("PACKET", delivery.packet)
        self.assertTrue(pilot.manipulation_check("control", delivery.receipt)["passed"])

        ablated = offered([pilot.INC_MEMORY], [pilot.LB_MEMORY], [pilot.LB_MEMORY])
        result = pilot.manipulation_check("lb-ablated", ablated)
        self.assertTrue(result["passed"])
        self.assertNotIn(pilot.LB_MEMORY, result["surfaced-ids"])
        inert = offered([pilot.INC_MEMORY], [pilot.LB_MEMORY], [])
        self.assertFalse(pilot.manipulation_check("lb-ablated", inert)["passed"])

    def test_wrong_uid_is_rejected_before_a_record_is_written(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            output = root / "must-not-exist.json"

            def recall(_problem: str, _arm: str):
                return pilot.RecallDelivery("PACKET", offered([pilot.LB_MEMORY, pilot.INC_MEMORY]))

            def wrapper(_problem: str, _base: str, _packet: str, receipt: Path):
                receipt.write_text(json.dumps(valid_isolation(1000)), encoding="utf-8")
                return subprocess.CompletedProcess([], 0, "stub ran as joe")

            with mock.patch.object(pilot, "RUNS_ROOT", root):
                with self.assertRaisesRegex(pilot.PilotError, "effective-uid 1000"):
                    pilot.run_one("a95J01", "51b6bc00", "control", output,
                                  recall_fn=recall, wrapper_fn=wrapper,
                                  reset_fn=lambda *_: None)
            self.assertFalse(output.exists(), "invalid isolation must never become data")

    def test_double_emitted_final_report_still_attributes(self) -> None:
        one = f"USED {pilot.LB_MEMORY}: applied\nIGNORED {pilot.INC_MEMORY}: unrelated\n"
        # codex exec prints its final message twice; the gate must not read that
        # as two verdicts per id.
        self.assertTrue(
            pilot.attribution_check(pilot.dedupe_verdict_lines("noise\n" + one + "noise\n" + one),
                                    [pilot.LB_MEMORY, pilot.INC_MEMORY])["passed"])
        conflicting = f"USED {pilot.LB_MEMORY}: applied\nIGNORED {pilot.LB_MEMORY}: no\n"
        self.assertFalse(
            pilot.attribution_check(pilot.dedupe_verdict_lines(conflicting),
                                    [pilot.LB_MEMORY])["passed"],
            "contradictory verdicts for one id must still fail")

    def test_runner_command_keeps_the_os_as_the_only_sandbox(self) -> None:
        command = pilot.wrapper_command("a95J01", "51b6bc00", "PACKET", Path("/tmp/r.json"))
        for flag in ("exec", "--ephemeral", "--ignore-user-config", "--ignore-rules"):
            self.assertIn(flag, command, "the dispatch wrapper refuses runs without this")
        index = command.index("--sandbox")
        self.assertEqual("danger-full-access", command[index + 1],
                         "codex's own bwrap sandbox cannot start on this host; the "
                         "isolated account's permissions are the sandbox")

    def test_failed_runner_is_named_not_laundered_into_an_empty_trace(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            output = root / "control.json"

            def recall(_problem: str, _arm: str):
                return pilot.RecallDelivery("PACKET", offered([pilot.LB_MEMORY, pilot.INC_MEMORY]))

            def wrapper(_problem: str, _base: str, _packet: str, receipt: Path):
                receipt.write_text(json.dumps(valid_isolation(runner_exit=127)),
                                   encoding="utf-8")
                return subprocess.CompletedProcess([], 127, "codex: command not found\n")

            with mock.patch.object(pilot, "RUNS_ROOT", root):
                with self.assertRaisesRegex(pilot.PilotError, "isolated runner failed"):
                    pilot.run_one("a95J01", "51b6bc00", "control", output,
                                  recall_fn=recall, wrapper_fn=wrapper,
                                  reset_fn=lambda *_: None)
            self.assertFalse(output.exists(), "a failed runner must never become data")
            transcript = output.with_name(output.name + ".wrapper.log")
            self.assertIn("command not found", transcript.read_text(encoding="utf-8"),
                          "the transcript must survive the rejection")

    def test_stub_runner_commits_and_e3_stable_extractor(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            run_dir = root / "a95J01"
            output = root / "control.json"
            extractor = root / "trace_stub.py"
            extractor.write_text(
                """#!/usr/bin/env python3
import argparse, hashlib, json, subprocess
p=argparse.ArgumentParser(); p.add_argument('repo'); p.add_argument('--base'); a=p.parse_args()
n=int(subprocess.check_output(['git','-C',a.repo,'rev-list','--count',a.base+'..HEAD']))
seq=[['stub','modify-body','success'] for _ in range(n)]
raw=json.dumps(seq,separators=(',',':')).encode()
print(json.dumps({'schema':'stub','attempt-count':n,'sequence':seq,'sha256':hashlib.sha256(raw).hexdigest()},sort_keys=True,separators=(',',':')))
""", encoding="utf-8")

            def recall(_problem: str, _arm: str):
                return pilot.RecallDelivery("PACKET", offered([pilot.LB_MEMORY, pilot.INC_MEMORY]))

            def wrapper(_problem: str, base: str, _packet: str, receipt: Path):
                run_dir.mkdir()
                subprocess.run(["git", "init", "-q"], cwd=run_dir, check=True)
                subprocess.run(["git", "config", "user.name", "stub"], cwd=run_dir, check=True)
                subprocess.run(["git", "config", "user.email", "stub@example.invalid"],
                               cwd=run_dir, check=True)
                (run_dir / "Main.lean").write_text("theorem t : True := by trivial\n")
                subprocess.run(["git", "add", "Main.lean"], cwd=run_dir, check=True)
                subprocess.run(["git", "commit", "-q", "-m", "baseline"], cwd=run_dir, check=True)
                subprocess.run(["git", "tag", base], cwd=run_dir, check=True)
                (run_dir / "Main.lean").write_text("theorem t : True := by exact True.intro\n")
                subprocess.run(["git", "add", "Main.lean"], cwd=run_dir, check=True)
                subprocess.run(["git", "commit", "-q", "-m", "attempt"], cwd=run_dir, check=True)
                receipt.write_text(json.dumps(valid_isolation()), encoding="utf-8")
                report = f"USED {pilot.LB_MEMORY}: applied\nUSED {pilot.INC_MEMORY}: checked\n"
                return subprocess.CompletedProcess([], 0, report)

            with mock.patch.object(pilot, "RUNS_ROOT", root):
                record = pilot.run_one(
                    "a95J01", "51b6bc00", "control", output,
                    recall_fn=recall, wrapper_fn=wrapper,
                    reset_fn=lambda *_: None, extractor=extractor,
                )
            self.assertTrue(record["accepted"])
            self.assertTrue(record["extractor-stable"])
            self.assertEqual(1, record["trace"]["attempt-count"])
            self.assertEqual((True, ""), e3.evaluate(record))

    def test_e3_names_nonstable_extractor_as_specific_defect(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            state = root / "counter"
            extractor = root / "unstable.py"
            extractor.write_text(
                f"""#!/usr/bin/env python3
import json, pathlib
p=pathlib.Path({str(state)!r}); n=int(p.read_text())+1 if p.exists() else 1; p.write_text(str(n))
print(json.dumps({{'attempt-count':1,'sha256':str(n)}}))
""", encoding="utf-8")
            trace, stable = pilot.extract_trace_twice(root, "base", extractor)
            record = {
                "isolation-receipt": {"effective-uid": pilot.EXPECTED_UID},
                "trace": trace, "extractor-stable": stable,
                "manipulation": {"passed": True},
                "attribution": {"passed": True},
            }
            passed, defect = e3.evaluate(record)
            self.assertFalse(passed)
            self.assertIn("not byte-stable", defect)


if __name__ == "__main__":
    unittest.main()
