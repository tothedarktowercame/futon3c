#!/usr/bin/env python3
"""
test_replay_state.py — B3-F6 tests for replay-based state derivation.
Mission: M-state-snapshot-witness.
Deposit grounding: ft-state-snapshot-witness-003 (b6 fact-lifecycle replay + b4 append-only
audit + b5 tri-store facts layer; run-14 h4's missing as-of-N view).

Tests cover:
  - Basic replay: latest state derived from log
  - As-of-N reconstruction: state at a past point in the log (run-14 h4)
  - Duplicate rejection: append-only audit rejects duplicate event-ids (b4)
  - State is DERIVED, never stored: mutating a derived result does not affect fresh replay

Runs from any cwd via: python3 test_replay_state.py
"""

import copy
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from replay_state import (
    SnapshotEvent, EventLog, DuplicateEventError,
    replay_state, replay_state_as_of,
)


class TestBasicReplay(unittest.TestCase):
    """Active state is derived by replaying the event log."""

    def test_latest_state_from_multiple_events(self):
        """Three inventory snapshots; replay returns the latest by timestamp."""
        log = EventLog()
        log.append(SnapshotEvent("boot-1", "inventory",
                                 {"families": 10}, "2026-07-10T10:00:00Z"))
        log.append(SnapshotEvent("boot-2", "inventory",
                                 {"families": 12}, "2026-07-10T11:00:00Z"))
        log.append(SnapshotEvent("boot-3", "inventory",
                                 {"families": 15}, "2026-07-10T12:00:00Z"))

        result = replay_state(log, "inventory")
        self.assertIsNotNone(result)
        self.assertEqual(result.state["families"], 15)
        self.assertEqual(result.event_id, "boot-3")

    def test_none_when_no_events(self):
        """Empty log => None (no active state)."""
        log = EventLog()
        result = replay_state(log, "inventory")
        self.assertIsNone(result)

    def test_none_when_no_events_for_container(self):
        """Events exist for other containers but not this one => None."""
        log = EventLog()
        log.append(SnapshotEvent("boot-1", "registry",
                                 {"families": 5}, "2026-07-10T10:00:00Z"))
        result = replay_state(log, "inventory")
        self.assertIsNone(result)

    def test_multiple_containers_independent(self):
        """Inventory and registry events coexist; replay picks the right one."""
        log = EventLog()
        log.append(SnapshotEvent("boot-1", "inventory",
                                 {"families": 10}, "2026-07-10T10:00:00Z"))
        log.append(SnapshotEvent("boot-1r", "registry",
                                 {"registered": 8}, "2026-07-10T10:00:00Z"))
        log.append(SnapshotEvent("boot-2", "inventory",
                                 {"families": 12}, "2026-07-10T11:00:00Z"))

        inv = replay_state(log, "inventory")
        reg = replay_state(log, "registry")
        self.assertEqual(inv.state["families"], 12)
        self.assertEqual(reg.state["registered"], 8)


class TestAsOfNReconstruction(unittest.TestCase):
    """
    As-of-N reconstruction (run-14 h4's missing VIEW, minimally realized):
    reconstruct 'state as of event N' by replaying only the prefix.
    """

    def test_state_at_boot_1(self):
        """Replay prefix [0,1) => state from boot-1 only."""
        log = EventLog()
        log.append(SnapshotEvent("boot-1", "inventory",
                                 {"families": 10}, "2026-07-10T10:00:00Z"))
        log.append(SnapshotEvent("boot-2", "inventory",
                                 {"families": 12}, "2026-07-10T11:00:00Z"))
        log.append(SnapshotEvent("boot-3", "inventory",
                                 {"families": 15}, "2026-07-10T12:00:00Z"))

        result = replay_state_as_of(log, "inventory", 1)
        self.assertIsNotNone(result)
        self.assertEqual(result.state["families"], 10)
        self.assertEqual(result.event_id, "boot-1")

    def test_state_at_boot_2(self):
        """Replay prefix [0,2) => latest from boot-1 and boot-2."""
        log = EventLog()
        log.append(SnapshotEvent("boot-1", "inventory",
                                 {"families": 10}, "2026-07-10T10:00:00Z"))
        log.append(SnapshotEvent("boot-2", "inventory",
                                 {"families": 12}, "2026-07-10T11:00:00Z"))
        log.append(SnapshotEvent("boot-3", "inventory",
                                 {"families": 15}, "2026-07-10T12:00:00Z"))

        result = replay_state_as_of(log, "inventory", 2)
        self.assertIsNotNone(result)
        self.assertEqual(result.state["families"], 12)
        self.assertEqual(result.event_id, "boot-2")

    def test_state_at_boot_3_full_log(self):
        """Replay prefix [0,3) = full log => same as replay_state."""
        log = EventLog()
        log.append(SnapshotEvent("boot-1", "inventory",
                                 {"families": 10}, "2026-07-10T10:00:00Z"))
        log.append(SnapshotEvent("boot-2", "inventory",
                                 {"families": 12}, "2026-07-10T11:00:00Z"))
        log.append(SnapshotEvent("boot-3", "inventory",
                                 {"families": 15}, "2026-07-10T12:00:00Z"))

        as_of = replay_state_as_of(log, "inventory", 3)
        latest = replay_state(log, "inventory")
        self.assertEqual(as_of.state["families"], latest.state["families"])

    def test_as_of_zero_returns_none(self):
        """Replay prefix [0,0) = empty => None."""
        log = EventLog()
        log.append(SnapshotEvent("boot-1", "inventory",
                                 {"families": 10}, "2026-07-10T10:00:00Z"))
        result = replay_state_as_of(log, "inventory", 0)
        self.assertIsNone(result)

    def test_as_of_interleaved_containers(self):
        """
        Events for multiple containers interleaved; as-of-N picks the right
        container's latest within the prefix.
        """
        log = EventLog()
        log.append(SnapshotEvent("boot-1", "inventory", {"v": 1}, "2026-07-10T10:00:00Z"))
        log.append(SnapshotEvent("boot-1r", "registry",  {"v": 1}, "2026-07-10T10:00:00Z"))
        log.append(SnapshotEvent("boot-2", "inventory",  {"v": 2}, "2026-07-10T11:00:00Z"))
        log.append(SnapshotEvent("boot-2r", "registry",  {"v": 2}, "2026-07-10T11:00:00Z"))
        log.append(SnapshotEvent("boot-3", "inventory",  {"v": 3}, "2026-07-10T12:00:00Z"))

        # As of index 3 (events 0,1,2): inventory latest = boot-2, registry latest = boot-1r
        inv = replay_state_as_of(log, "inventory", 3)
        reg = replay_state_as_of(log, "registry", 3)
        self.assertEqual(inv.state["v"], 2)
        self.assertEqual(reg.state["v"], 1)


class TestDuplicateRejection(unittest.TestCase):
    """
    Duplicate event-ids are REJECTED (b4 append-only audit).
    The log never silently swallows a duplicate — it raises.
    """

    def test_duplicate_event_id_rejected(self):
        """Appending an event with a duplicate event_id raises DuplicateEventError."""
        log = EventLog()
        log.append(SnapshotEvent("boot-1", "inventory",
                                 {"families": 10}, "2026-07-10T10:00:00Z"))

        with self.assertRaises(DuplicateEventError) as ctx:
            log.append(SnapshotEvent("boot-1", "inventory",
                                     {"families": 99}, "2026-07-10T11:00:00Z"))
        self.assertIn("boot-1", str(ctx.exception))
        self.assertIn("duplicate rejected", str(ctx.exception))

    def test_log_unchanged_after_rejected_duplicate(self):
        """After a duplicate is rejected, the log still has the original event only."""
        log = EventLog()
        log.append(SnapshotEvent("boot-1", "inventory",
                                 {"families": 10}, "2026-07-10T10:00:00Z"))
        try:
            log.append(SnapshotEvent("boot-1", "inventory",
                                     {"families": 99}, "2026-07-10T11:00:00Z"))
        except DuplicateEventError:
            pass

        self.assertEqual(len(log), 1)
        result = replay_state(log, "inventory")
        self.assertEqual(result.state["families"], 10)

    def test_different_containers_same_id_rejected(self):
        """Same event_id on different containers is still a duplicate."""
        log = EventLog()
        log.append(SnapshotEvent("shared-id", "inventory",
                                 {"v": 1}, "2026-07-10T10:00:00Z"))
        with self.assertRaises(DuplicateEventError):
            log.append(SnapshotEvent("shared-id", "registry",
                                     {"v": 2}, "2026-07-10T10:00:00Z"))


class TestStateIsDerived(unittest.TestCase):
    """
    State is NEVER stored — it is ALWAYS derived by replay.
    Mutating a derived copy does not affect a fresh replay.
    This is the fact-lifecycle discipline (b6): active state is derived from
    the log, not stored as a mutable field that could drift.
    """

    def test_mutating_derived_state_does_not_affect_replay(self):
        """
        Get the derived state, mutate it, then replay again.
        The fresh replay must see the ORIGINAL state, not the mutation.
        Proves state is derived (from the immutable log), not stored (in a mutable cache).
        """
        log = EventLog()
        log.append(SnapshotEvent("boot-1", "inventory",
                                 {"families": 10, "list": [1, 2, 3]},
                                 "2026-07-10T10:00:00Z"))

        # First replay — get derived state
        result1 = replay_state(log, "inventory")
        self.assertEqual(result1.state["families"], 10)

        # Mutate the derived state
        result1.state["families"] = 999
        result1.state["list"].append(4)

        # Second replay — must see ORIGINAL state, not the mutation
        result2 = replay_state(log, "inventory")
        self.assertEqual(result2.state["families"], 10,
                         "fresh replay must see original 10, not mutated 999")
        self.assertEqual(result2.state["list"], [1, 2, 3],
                         "fresh replay must see original list, not mutated")

    def test_replay_is_deterministic(self):
        """Multiple replays of the same log produce identical results."""
        log = EventLog()
        log.append(SnapshotEvent("boot-1", "inventory",
                                 {"families": 10}, "2026-07-10T10:00:00Z"))
        log.append(SnapshotEvent("boot-2", "inventory",
                                 {"families": 12}, "2026-07-10T11:00:00Z"))

        r1 = replay_state(log, "inventory")
        r2 = replay_state(log, "inventory")
        r3 = replay_state(log, "inventory")

        self.assertEqual(r1.state, r2.state)
        self.assertEqual(r2.state, r3.state)
        self.assertEqual(r1.event_id, r2.event_id)
        self.assertEqual(r2.event_id, r3.event_id)


if __name__ == "__main__":
    unittest.main(verbosity=2)
