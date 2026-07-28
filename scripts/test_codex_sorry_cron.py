#!/usr/bin/env python3

import fcntl
import importlib.util
import json
import os
import tempfile
import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest import mock

from edn_format import Keyword as K


SCRIPT = Path(__file__).with_name("codex_sorry_cron.py")
SPEC = importlib.util.spec_from_file_location("codex_sorry_cron", SCRIPT)
cron = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(cron)


def rate_event(timestamp, used):
    return {
        "timestamp": timestamp.isoformat().replace("+00:00", "Z"),
        "type": "event_msg",
        "payload": {
            "type": "token_count",
            "rate_limits": {"primary": {"used_percent": used}},
        },
    }


def row(row_id="r1", status="untouched", file="ConstructionTargets/X.lean"):
    return {
        K("id"): row_id,
        K("kind"): K("hard-proof-step"),
        K("file"): file,
        K("line"): 3,
        K("statement-hint"): "theorem x : True",
        K("unblocks"): [],
        K("status"): K(status),
        K("job-id"): None,
        K("dispatched-at"): None,
        K("resolved-at"): None,
        K("outcome"): None,
    }


class UsageGateTests(unittest.TestCase):
    def write_session(self, root, name, events):
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("\n".join(json.dumps(event) for event in events) + "\n")
        return path

    def test_newest_rate_limit_wins_across_recent_files(self):
        now = datetime(2026, 7, 28, 12, tzinfo=timezone.utc)
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            old = now - timedelta(hours=2)
            new = now - timedelta(minutes=2)
            self.write_session(root, "a.jsonl", [rate_event(old, 30)])
            self.write_session(root, "b.jsonl", [rate_event(new, 12)])
            snapshot = cron.newest_rate_limit(root, now)
            self.assertEqual(12.0, snapshot["used_percent"])
            self.assertEqual(120.0, snapshot["age_seconds"])

    def test_threshold_is_strict(self):
        with mock.patch.object(cron, "MAX_USED_PERCENT", 50):
            cron.enforce_usage({"used_percent": 49.9})
            with self.assertRaises(cron.GateClosed):
                cron.enforce_usage({"used_percent": 50.0})

    def test_stale_signal_fails_closed(self):
        now = datetime(2026, 7, 28, 12, tzinfo=timezone.utc)
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            path = self.write_session(
                root, "stale.jsonl", [rate_event(now - timedelta(hours=25), 0)]
            )
            os.utime(path, (now.timestamp(), now.timestamp()))
            with self.assertRaisesRegex(cron.GateClosed, "stale-rate-limit"):
                cron.newest_rate_limit(root, now)

    def test_absent_signal_fails_closed(self):
        with tempfile.TemporaryDirectory() as tmp:
            with self.assertRaisesRegex(cron.GateClosed, "no-recent-session-files"):
                cron.newest_rate_limit(Path(tmp), datetime.now(timezone.utc))


class AgencyGateTests(unittest.TestCase):
    @staticmethod
    def agent(status="restored", ready=True):
        return {
            "type": "codex",
            "status": status,
            "invoke-ready?": ready,
            "invoke-route": "local",
            "metadata": {},
        }

    def test_concurrency_and_preferred_pool(self):
        agents = {
            "codex-4": self.agent(),
            "codex-5": self.agent(),
            "codex-8": self.agent(),
            "codex-6": self.agent(),
            "codex-2": self.agent(status="invoking"),
        }
        with mock.patch.object(cron, "MAX_OTHER_INVOKING", 1):
            self.assertEqual(("codex-6", 1), cron.choose_agent(agents))

    def test_concurrency_fails_closed_above_bound(self):
        agents = {
            "codex-1": self.agent(status="invoking"),
            "codex-2": self.agent(status="invoking"),
            "codex-6": self.agent(),
        }
        with (
            mock.patch.object(cron, "MAX_OTHER_INVOKING", 1),
            self.assertRaisesRegex(cron.GateClosed, "concurrency-gate-closed"),
        ):
            cron.choose_agent(agents)


class QueueGateTests(unittest.TestCase):
    def test_backpressure_blocks_any_dispatched_row(self):
        with self.assertRaisesRegex(cron.GateClosed, "verification-backpressure"):
            cron.enforce_backpressure([row("done", "solved"), row("pending", "dispatched")])

    def test_queue_order_and_zai_live_problem_guard(self):
        queue = [
            row("first", file="problems/a00J01/lean/Main.lean"),
            row("second", file="problems/a00J02/lean/Main.lean"),
        ]
        index, chosen = cron.choose_row(queue, {"a00J01"})
        self.assertEqual(1, index)
        self.assertEqual("second", chosen[K("id")])

    def test_seed_starts_with_fixed_prereg_and_has_no_scaffolds(self):
        queue = cron.load_queue(cron.QUEUE_PATH)
        self.assertEqual(
            [
                "schwarz-equality-case",
                "connectedComponents-complement-lemniscate",
                "rouche-root-count-transfer",
                "radial-integration-r3",
            ],
            [item[K("id")] for item in queue[:4]],
        )
        self.assertNotIn(":scaffold", {str(item[K("kind")]) for item in queue})


class RuntimeDisciplineTests(unittest.TestCase):
    def test_flock_exclusivity_returns_without_evaluating_gates(self):
        with tempfile.TemporaryDirectory() as tmp:
            lock_path = Path(tmp) / "cron.lock"
            lock_path.touch()
            with lock_path.open("a+") as held:
                fcntl.flock(held, fcntl.LOCK_EX | fcntl.LOCK_NB)
                with (
                    mock.patch.object(cron, "LOCK_PATH", lock_path),
                    mock.patch.object(cron, "newest_rate_limit") as usage,
                    mock.patch.object(cron, "emit"),
                ):
                    self.assertEqual(0, cron.run(dry_run=True))
                    usage.assert_not_called()

    def test_dry_run_exercises_gates_without_queue_progress_or_dispatch_writes(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            lock = root / "cron.lock"
            template = root / "template.txt"
            template.write_text("@@ID@@ @@FILE@@ @@STATEMENT@@")
            queue = [row()]
            roster = {
                "ok": True,
                "agents": {"codex-6": AgencyGateTests.agent()},
            }
            with (
                mock.patch.object(cron, "LOCK_PATH", lock),
                mock.patch.object(cron, "TEMPLATE_PATH", template),
                mock.patch.object(
                    cron,
                    "newest_rate_limit",
                    return_value={"used_percent": 1.0, "age_seconds": 1.0},
                ),
                mock.patch.object(cron, "get_json", return_value=roster),
                mock.patch.object(cron, "load_queue", return_value=queue),
                mock.patch.object(cron, "save_queue") as save,
                mock.patch.object(cron, "append_progress") as progress,
                mock.patch.object(cron, "dispatch") as dispatch,
                mock.patch.object(cron, "LOG_PATH", root / "must-not-exist.log"),
            ):
                self.assertEqual(0, cron.run(dry_run=True))
                save.assert_not_called()
                progress.assert_not_called()
                dispatch.assert_not_called()
                self.assertEqual("untouched", cron.status_name(queue[0]))
                self.assertFalse((root / "must-not-exist.log").exists())


if __name__ == "__main__":
    unittest.main()
