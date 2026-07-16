#!/usr/bin/env python3
"""
reclock_decision.py — B3-F3 pure decision function for autoclock edit-activity reclock.
Mission: M-autoclock-in (INSTANTIATE-3 edit-activity reclock).
Deposit grounding: ft-autoclock-in-002 (guai/xun/sui calibration risks + scheduled-observer no-op trap).

Implements the PURE decision: save-event timeline -> promote/hold.
No side effects, no I/O, no clock mutation — just the decision.

Rules (per mission doc INSTANTIATE-3):
  1. A save lands on a file whose path resolves by exact ID to an existing C/M/E candidate.
  2. The file has accrued >= N saves within window W.
  3. X differs from the current clock, and X is the DOMINANT recently-edited target
     (hysteresis: if two C/M/E files are edited alternately, require clear dominance — no thrash).
  4. Records an audit witness and remains hydra-overridable (out of scope for the pure function).

Thresholds: N=3, W=600s — UNCALIBRATED v0 (per mission doc: "reuse INSTANTIATE-3's 3 / 600s as
a start; calibrate to agent edit cadence"). These are placeholder defaults, NOT empirically tuned.

Calibration risks the decision function is designed against:
  - GUAI OVERSHOOT (burst-thrash): a burst of saves on mission B must not switch the clock off
    an actively-dominant mission A. Dominance + hysteresis prevents this.
  - XUN DRIFT (excess gentleness): sparse trickle saves (1 save per W) never cross N-within-W,
    so the clock doesn't reclock. Correct: low-amplitude signal = HOLD.
  - SUI NO-OP TRAP (scheduled-observer): if the timeline is unchanged since the last decision,
    the function returns HOLD, never a promote that moves nothing. Liveness requires state-delta.
"""

from dataclasses import dataclass, field
from typing import Optional

# ── UNCALIBRATED v0 thresholds (per mission doc INSTANTIATE-3) ──────────────
# "reuse INSTANTIATE-3's 3 / 600s as a start; calibrate to agent edit cadence"
DEFAULT_N = 3          # minimum saves within window to consider promotion
DEFAULT_W = 600        # window in seconds (10 minutes)
DEFAULT_HYSTERESIS_MARGIN = 2  # dominant target's count must exceed next by this margin


@dataclass(frozen=True)
class SaveEvent:
    """A single save event on a mission/campaign/excursion file."""
    target: str          # the C/M/E ID resolved from the file path (e.g. "M-autoclock-in")
    timestamp: float     # epoch seconds


@dataclass(frozen=True)
class ReclockDecision:
    """The output of the pure decision function."""
    action: str           # "promote" or "hold"
    target: Optional[str] = None  # the target to promote to (None if hold)
    rule: str = ""        # which rule fired (for audit witness)
    reason: str = ""      # human-readable reason (for audit witness)

    def __str__(self):
        if self.action == "promote":
            return f"promote -> {self.target} ({self.rule}: {self.reason})"
        return f"hold ({self.reason})"


def _counts_within_window(events, current_time, window):
    """
    Count saves per target within [current_time - window, current_time].
    Returns dict {target: count}, sorted by count descending.
    """
    cutoff = current_time - window
    counts = {}
    for ev in events:
        if ev.timestamp >= cutoff and ev.timestamp <= current_time:
            counts[ev.target] = counts.get(ev.target, 0) + 1
    return counts


def _top_two(counts):
    """Return ((top_target, top_count), (second_target, second_count) or None)."""
    if not counts:
        return None, None
    sorted_targets = sorted(counts.items(), key=lambda x: (-x[1], x[0]))
    top = sorted_targets[0]
    second = sorted_targets[1] if len(sorted_targets) > 1 else None
    return top, second


def reclock_decision(
    events: list[SaveEvent],
    current_clock: Optional[str],
    current_time: float,
    n_threshold: int = DEFAULT_N,
    window: int = DEFAULT_W,
    hysteresis_margin: int = DEFAULT_HYSTERESIS_MARGIN,
) -> ReclockDecision:
    """
    Pure decision function: given a save-event timeline, the current clock target,
    and the current time, decide whether to promote (switch clock) or hold.

    Parameters:
        events:          full save-event timeline (all events ever recorded)
        current_clock:   the currently-clocked target (M-foo), or None if at floor
        current_time:    epoch seconds (the "now" for window calculation)
        n_threshold:     minimum saves within window to consider promotion
        window:          window size in seconds
        hysteresis_margin: dominant target's count must exceed next by this margin

    Returns:
        ReclockDecision with action="promote" or "hold"
    """
    counts = _counts_within_window(events, current_time, window)
    top, second = _top_two(counts)

    # ── NO-OP TRAP (sui/scheduled-observer): unchanged timeline => HOLD ──────
    # If no events fall within the window, there is no state-delta.
    # A promote that moves nothing is the scheduled-observer's no-op trap.
    if top is None:
        return ReclockDecision(
            action="hold",
            rule="no-op-trap",
            reason="no saves within window — no state-delta, nothing to promote",
        )

    top_target, top_count = top

    # ── XUN DRIFT (excess gentleness): below N threshold => HOLD ─────────────
    # Sparse trickle saves (e.g. 1 save per W) never cross N-within-W.
    # Low-amplitude signal does not reclock — this is correct, not a failure.
    if top_count < n_threshold:
        return ReclockDecision(
            action="hold",
            rule="below-threshold",
            reason=f"top target {top_target} has {top_count} saves (< N={n_threshold}) — "
                   f"sparse signal, gentleness threshold not crossed",
        )

    # ── GUAI OVERSHOOT (burst-thrash): insufficient dominance => HOLD ────────
    # Even if top reaches N, if the second target is close (within hysteresis_margin),
    # switching would risk thrashing between alternately-edited targets.
    if second is not None:
        second_target, second_count = second
        if top_count - second_count < hysteresis_margin:
            return ReclockDecision(
                action="hold",
                rule="insufficient-dominance",
                reason=f"top {top_target} ({top_count}) vs second {second_target} ({second_count}) "
                       f"— margin {top_count - second_count} < {hysteresis_margin}, "
                       f"switching would risk thrash",
            )

    # ── Already clocked on this target => HOLD ───────────────────────────────
    if top_target == current_clock:
        return ReclockDecision(
            action="hold",
            rule="already-clocked",
            reason=f"top target {top_target} is already the current clock",
        )

    # ── PROMOTE ──────────────────────────────────────────────────────────────
    # All conditions met: N saves within W, dominant by margin, differs from current clock.
    return ReclockDecision(
        action="promote",
        target=top_target,
        rule="edit-activity",
        reason=f"{top_count} saves within {window}s, dominant by margin "
               f">= {hysteresis_margin} over next target",
    )
