#!/usr/bin/env python3
"""
replay_state.py — B3-F6 replay-based state derivation for the snapshot witness system.
Mission: M-state-snapshot-witness.
Deposit grounding: ft-state-snapshot-witness-003 (b6 fact-lifecycle replay + b4 append-only
audit + b5 tri-store facts layer; run-14 h4 named the missing as-of-N view).

The core discipline: state is NEVER stored — it is ALWAYS derived by replaying
the append-only event log. This is the fact-lifecycle pattern (b6): active state
is the latest projection from the event log, not a mutable field that could drift.
And the tri-store facts layer (b5): the append-only event log IS the single source
of truth; derived state is a meme (rebuilt from facts, never authoritative alone).

Implements:
  - SnapshotEvent: an append-only event record (container, state, emitted-at, event-id)
  - replay_state: derive the active state for a container by replaying events
  - replay_state_as_of: reconstruct "state as of event N" (run-14 h4's missing VIEW)
  - DuplicateEventError: raised when a duplicate event-id is appended (b4 audit)

stdlib only; no I/O, no persistence — the event log is passed as a list.
"""

import copy
import hashlib
from dataclasses import dataclass, field
from typing import Optional, Any


class DuplicateEventError(Exception):
    """Raised when an event with a duplicate event-id is appended to the log.

    This is the append-only audit discipline (b4): the store boundary rejects
    duplicate ids. The log never silently swallows a duplicate — it raises,
    so the caller knows the append was rejected.
    """
    pass


@dataclass(frozen=True)
class SnapshotEvent:
    """
    A single append-only snapshot event in the facts layer (b5 tri-store).

    Each event records a point-in-time projection of a container's state.
    The event-id is unique within the log; duplicates are rejected by the audit.

    Note: the state field is a frozen reference but the object it points to
    may be mutable (dict, list). replay_state returns a deep copy of state
    so mutations to derived results don't propagate back to the log.
    """
    event_id: str          # unique identifier (e.g. "boot-2026-07-10T12:00:00Z")
    container: str         # e.g. "inventory", "registry", "repo-refs", "hud-render"
    state: Any             # the projected state (a dict, list, or any serialisable)
    emitted_at: str        # ISO-8601 timestamp when the snapshot was emitted


class EventLog:
    """
    An append-only event log with duplicate rejection (b4 append-only audit).

    Events are never edited or deleted. Mistakes are corrected by appending
    a new event, not by mutating a prior one. The log is the single source
    of truth (b5 tri-store facts layer).
    """

    def __init__(self):
        self._events: list[SnapshotEvent] = []
        self._seen_ids: set[str] = set()

    def append(self, event: SnapshotEvent) -> None:
        """Append an event. Raises DuplicateEventError if event_id is already in the log."""
        if event.event_id in self._seen_ids:
            raise DuplicateEventError(
                f"event_id '{event.event_id}' already exists in the log — "
                f"duplicate rejected (append-only audit: the log never silently "
                f"swallows a duplicate)"
            )
        self._seen_ids.add(event.event_id)
        self._events.append(event)

    def events(self) -> list[SnapshotEvent]:
        """Return a copy of the event list (immutable from outside)."""
        return list(self._events)

    def __len__(self):
        return len(self._events)


def replay_state(log: EventLog, container: str) -> Optional[SnapshotEvent]:
    """
    Derive the active state for a container by replaying the event log.

    The active state is the LATEST event for the given container — derived by
    replay, never stored. This is the fact-lifecycle pattern (b6): active state
    is derived from the log by replay, not stored as a separate mutable field.

    Returns a SnapshotEvent with a DEEP COPY of the state, so mutations to the
    derived result do not propagate back to the log's stored event. This enforces
    the "state is derived, never stored" discipline: the caller gets a fresh copy
    each time; the log's events are immutable.

    Returns None if no events exist for that container.
    """
    latest = None
    for event in log.events():
        if event.container == container:
            if latest is None or event.emitted_at > latest.emitted_at:
                latest = event
    if latest is None:
        return None
    # Return a deep copy so the caller cannot mutate the log's stored state
    return SnapshotEvent(
        event_id=latest.event_id,
        container=latest.container,
        state=copy.deepcopy(latest.state),
        emitted_at=latest.emitted_at,
    )


def replay_state_as_of(log: EventLog, container: str, as_of_index: int) -> Optional[SnapshotEvent]:
    """
    Reconstruct the state of a container AS OF event index N (run-14 h4's missing VIEW).

    Given the event log, replay only the first N events and return the latest
    state for the container within that prefix. This is the time-travel query:
    "what was the inventory state at boot N?"

    Parameters:
        log:           the append-only event log
        container:     the container to query (e.g. "inventory")
        as_of_index:   replay only events [0, as_of_index) — the prefix length

    Returns the latest SnapshotEvent for the container within the prefix,
    or None if no events for that container exist in the prefix.
    """
    events = log.events()
    prefix = events[:as_of_index]

    latest = None
    for event in prefix:
        if event.container == container:
            if latest is None or event.emitted_at > latest.emitted_at:
                latest = event
    if latest is None:
        return None
    # Return a deep copy (same discipline as replay_state)
    return SnapshotEvent(
        event_id=latest.event_id,
        container=latest.container,
        state=copy.deepcopy(latest.state),
        emitted_at=latest.emitted_at,
    )
