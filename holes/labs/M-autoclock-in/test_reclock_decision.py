#!/usr/bin/env python3
"""
test_reclock_decision.py — B3-F3 tests for the autoclock reclock decision function.
Mission: M-autoclock-in.
Deposit grounding: ft-autoclock-in-002 (guai/xun/sui calibration risks + scheduled-observer no-op trap).

Tests cover the mission's named calibration risks:
  - burst-thrash (guai overshoot)
  - drift-under-gentleness (xun)
  - no-op trap (sui/scheduled-observer)
Plus baseline tests: basic promote, already-clocked hold.

Runs from any cwd via: python3 test_reclock_decision.py
"""

import os
import sys
import unittest

# Add the lab dir to path so we can import reclock_decision regardless of cwd
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from reclock_decision import (
    SaveEvent, ReclockDecision, reclock_decision,
    DEFAULT_N, DEFAULT_W, DEFAULT_HYSTERESIS_MARGIN,
)


class TestBasicPromote(unittest.TestCase):
    """Baseline: sustained edits to M-foo at the floor => promote to M-foo."""

    def test_promote_at_floor(self):
        """3 saves on M-foo within window, no current clock => promote."""
        events = [
            SaveEvent("M-foo", 100.0),
            SaveEvent("M-foo", 200.0),
            SaveEvent("M-foo", 300.0),
        ]
        result = reclock_decision(events, current_clock=None, current_time=350.0)
        self.assertEqual(result.action, "promote")
        self.assertEqual(result.target, "M-foo")
        self.assertEqual(result.rule, "edit-activity")

    def test_promote_switches_stale_clock(self):
        """3 saves on M-foo while clocked on M-stale => promote (switch-not-floor)."""
        events = [
            SaveEvent("M-foo", 100.0),
            SaveEvent("M-foo", 200.0),
            SaveEvent("M-foo", 300.0),
        ]
        result = reclock_decision(events, current_clock="M-stale", current_time=350.0)
        self.assertEqual(result.action, "promote")
        self.assertEqual(result.target, "M-foo")

    def test_already_clocked_hold(self):
        """3 saves on M-foo while already clocked on M-foo => hold."""
        events = [
            SaveEvent("M-foo", 100.0),
            SaveEvent("M-foo", 200.0),
            SaveEvent("M-foo", 300.0),
        ]
        result = reclock_decision(events, current_clock="M-foo", current_time=350.0)
        self.assertEqual(result.action, "hold")
        self.assertEqual(result.rule, "already-clocked")


class TestGuaiBurstThrash(unittest.TestCase):
    """
    GUAI OVERSHOOT: a burst of saves on mission B must NOT thrash the clock
    off an actively-dominant mission A.

    Scenario: A has been actively edited (5 saves) but B just got a burst
    (3 saves, reaching threshold). Without the dominance check, B would
    promote and switch the clock off A. With hysteresis, A's dominance
    (5 vs 3, margin 2) holds — B's burst does not thrash.
    """

    def test_burst_on_b_does_not_thrash_off_dominant_a(self):
        """A has 5 saves, B has 3 saves (burst) — A dominant, no switch to B."""
        events = [
            SaveEvent("M-aaa", 100.0),
            SaveEvent("M-aaa", 150.0),
            SaveEvent("M-aaa", 200.0),
            SaveEvent("M-aaa", 250.0),
            SaveEvent("M-aaa", 280.0),
            # B's burst (reaches N=3 but not dominant)
            SaveEvent("M-bbb", 290.0),
            SaveEvent("M-bbb", 295.0),
            SaveEvent("M-bbb", 300.0),
        ]
        # Clocked on M-aaa; B's burst must not switch
        result = reclock_decision(events, current_clock="M-aaa", current_time=310.0)
        self.assertEqual(result.action, "hold")
        self.assertEqual(result.rule, "already-clocked",
                         "M-aaa is still dominant AND already clocked — no reason to switch")

    def test_burst_on_b_reaches_threshold_but_marginal_dominance(self):
        """
        A has 4 saves, B has 3 saves — margin is 1, less than hysteresis_margin (2).
        Even though A is technically "top", the margin is too thin to act on.
        Clock should HOLD (not switch to A — A is top but marginal).
        """
        events = [
            SaveEvent("M-aaa", 100.0),
            SaveEvent("M-aaa", 200.0),
            SaveEvent("M-aaa", 250.0),
            SaveEvent("M-aaa", 280.0),
            SaveEvent("M-bbb", 285.0),
            SaveEvent("M-bbb", 290.0),
            SaveEvent("M-bbb", 300.0),
        ]
        # Clocked on M-ccc (neither A nor B); A is top (4) but margin over B (3) is 1 < 2
        result = reclock_decision(events, current_clock="M-ccc", current_time=310.0)
        self.assertEqual(result.action, "hold")
        self.assertEqual(result.rule, "insufficient-dominance",
                         "A (4) vs B (3) margin=1 < hysteresis_margin=2 — no switch, no thrash")


class TestXunDriftUnderGentleness(unittest.TestCase):
    """
    XUN EXCESS GENTLENESS: sparse trickle saves (1 save per window) must NOT reclock.
    Low amplitude that never crosses N-within-W is correctly held.
    The clock stays where it is; the trickle is too gentle to move it.
    """

    def test_single_save_does_not_reclock(self):
        """1 save on M-foo, threshold N=3 => hold (below threshold)."""
        events = [
            SaveEvent("M-foo", 100.0),
        ]
        result = reclock_decision(events, current_clock="M-stale", current_time=200.0)
        self.assertEqual(result.action, "hold")
        self.assertEqual(result.rule, "below-threshold")

    def test_two_saves_does_not_reclock(self):
        """2 saves on M-foo, threshold N=3 => hold (below threshold)."""
        events = [
            SaveEvent("M-foo", 100.0),
            SaveEvent("M-foo", 200.0),
        ]
        result = reclock_decision(events, current_clock="M-stale", current_time=250.0)
        self.assertEqual(result.action, "hold")
        self.assertEqual(result.rule, "below-threshold")

    def test_trickle_saves_spread_outside_window(self):
        """
        3 saves on M-foo but spread over > W seconds — only 1-2 fall within
        any given window. The clock does not reclock because the signal is
        too sparse in time.
        """
        events = [
            SaveEvent("M-foo", 100.0),   # outside window [700-1300]
            SaveEvent("M-foo", 400.0),   # outside window [700-1300]
            SaveEvent("M-foo", 700.0),   # at edge of window
        ]
        # current_time=1300, window=600 => cutoff=700. Only 1 event (at 700) in window.
        result = reclock_decision(events, current_clock="M-stale", current_time=1300.0)
        self.assertEqual(result.action, "hold")
        self.assertEqual(result.rule, "below-threshold",
                         "spread-out saves = only 1 in window — xun drift, too gentle")


class TestSuiNoOpTrap(unittest.TestCase):
    """
    SUI / SCHEDULED-OBSERVER NO-OP TRAP: if the timeline is unchanged since
    the last decision, the function returns HOLD, never a promote that moves
    nothing. Liveness requires state-delta; firing without moving is the trap.
    """

    def test_empty_timeline_holds(self):
        """No events at all => hold (no-op trap)."""
        result = reclock_decision([], current_clock="M-foo", current_time=300.0)
        self.assertEqual(result.action, "hold")
        self.assertEqual(result.rule, "no-op-trap")

    def test_no_events_in_window_holds(self):
        """Events exist but all outside the window => hold (no-op trap)."""
        events = [
            SaveEvent("M-foo", 100.0),   # far outside window
        ]
        # current_time=1000, window=600 => cutoff=400. Event at 100 is outside.
        result = reclock_decision(events, current_clock="M-bar", current_time=1000.0)
        self.assertEqual(result.action, "hold")
        self.assertEqual(result.rule, "no-op-trap",
                         "no state-delta within window — firing would be a no-op with a green light")

    def test_unchanged_timeline_holds(self):
        """
        Same events, same clock, called again => hold. The scheduled observer
        fires but nothing has changed since last time — HOLD is correct, not
        a redundant promote.
        """
        events = [
            SaveEvent("M-foo", 100.0),
            SaveEvent("M-foo", 200.0),
            SaveEvent("M-foo", 300.0),
        ]
        # First call promotes
        result1 = reclock_decision(events, current_clock=None, current_time=350.0)
        self.assertEqual(result1.action, "promote")

        # Second call with same timeline, now clocked on M-foo => hold (no new saves)
        result2 = reclock_decision(events, current_clock="M-foo", current_time=360.0)
        self.assertEqual(result2.action, "hold")
        self.assertEqual(result2.rule, "already-clocked",
                         "no new saves since last decision — already clocked, no-op")


class TestThresholdsDocumented(unittest.TestCase):
    """Verify thresholds are the documented UNCALIBRATED v0 values."""

    def test_default_n_is_3(self):
        self.assertEqual(DEFAULT_N, 3)

    def test_default_w_is_600(self):
        self.assertEqual(DEFAULT_W, 600)

    def test_thresholds_are_uncalibrated_v0(self):
        """
        The mission doc says: 'reuse INSTANTIATE-3's 3 / 600s as a start;
        calibrate to agent edit cadence.' These are UNCALIBRATED placeholders.
        This test documents that fact and will FAIL if someone changes the
        defaults without updating this test — forcing a conscious decision.
        """
        # If you change these defaults, you MUST update this test AND document
        # the calibration source (empirical measurement, agent cadence study, etc.)
        self.assertEqual(DEFAULT_N, 3, "UNCALIBRATED v0 — per mission doc INSTANTIATE-3")
        self.assertEqual(DEFAULT_W, 600, "UNCALIBRATED v0 — per mission doc INSTANTIATE-3 (10 min)")
        self.assertEqual(DEFAULT_HYSTERESIS_MARGIN, 2,
                         "UNCALIBRATED v0 — prevents thrash but value not empirically tuned")


if __name__ == "__main__":
    unittest.main(verbosity=2)
