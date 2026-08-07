"""Tests for the pass-1 lane: outcome gating, claim verification, quota waits."""

from __future__ import annotations

import sys
from pathlib import Path

import pytest

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

import statement_campaign as sc  # noqa: E402


BANK = "theorem apm_x00a01 (n : Nat) : n + 0 = n := by\n  sorry\n"


@pytest.fixture()
def repo(tmp_path, monkeypatch):
    """A throwaway repo + manifest/ledger so tests never touch real state."""

    problem = tmp_path / "problems" / "x00A01" / "lean"
    problem.mkdir(parents=True)
    (problem / "Main.lean").write_text(BANK, encoding="utf-8")
    monkeypatch.setattr(sc, "REPO", tmp_path)
    monkeypatch.setattr(sc, "MANIFEST", tmp_path / "manifest.jsonl")
    monkeypatch.setattr(sc, "LEDGER", tmp_path / "ledger.jsonl")
    return tmp_path


def _freeze(repo: Path, pid: str = "x00A01") -> str:
    source = (repo / "problems" / pid / "lean" / "Main.lean").read_text()
    _name, _norm, digest = sc.gates.statement_hash(source, pid)
    sc.append_jsonl(sc.MANIFEST, {"at": "t", "problem-id": pid,
                                  "status": "approved", "statement-hash": digest})
    return digest


def _stub_build(monkeypatch, exit_code: int = 0) -> None:
    monkeypatch.setattr(sc.gates, "_run_lean",
                        lambda *a, **k: {"exit-code": exit_code, "stderr-tail": ""})


def _stub_declared(monkeypatch, value):
    monkeypatch.setattr(sc, "declared_outcome", lambda pid: value)


def test_frozen_hash_returns_latest_recorded(repo):
    digest = _freeze(repo)
    assert sc.frozen_hash("x00A01") == digest
    assert sc.frozen_hash("nope") is None


def test_substituted_statement_is_void_even_with_a_clean_build(repo, monkeypatch):
    """The a98A01 failure mode: the build says nothing about fidelity."""

    _freeze(repo)
    _stub_build(monkeypatch, 0)
    _stub_declared(monkeypatch, "proved")
    (repo / "problems" / "x00A01" / "lean" / "Main.lean").write_text(
        "theorem apm_x00a01 (n : Nat) : n = n := by\n  rfl\n", encoding="utf-8")
    res = sc.pass1_gate("x00A01")
    assert res["outcome"] == "void-statement-changed"
    assert res["reasons"] == ["hash-moved"]


def test_claimed_proved_with_sorries_is_downgraded(repo, monkeypatch):
    _freeze(repo)
    _stub_build(monkeypatch, 0)
    _stub_declared(monkeypatch, "proved")
    res = sc.pass1_gate("x00A01")  # artifact still holds its sorry
    assert res["outcome"] == "partial"
    assert res["reasons"] == ["claimed-proved-but-1-sorries"]


def test_claimed_partial_with_no_sorries_is_upgraded(repo, monkeypatch):
    _freeze(repo)
    _stub_build(monkeypatch, 0)
    _stub_declared(monkeypatch, "partial")
    (repo / "problems" / "x00A01" / "lean" / "Main.lean").write_text(
        BANK.replace("  sorry\n", "  simp\n"), encoding="utf-8")
    res = sc.pass1_gate("x00A01")
    assert res["outcome"] == "proved"


def test_honest_skip_is_recorded_as_skipped(repo, monkeypatch):
    _freeze(repo)
    _stub_build(monkeypatch, 0)
    _stub_declared(monkeypatch, "skipped")
    assert sc.pass1_gate("x00A01")["outcome"] == "skipped"


def test_build_failure_beats_any_claim(repo, monkeypatch):
    _freeze(repo)
    _stub_build(monkeypatch, 1)
    _stub_declared(monkeypatch, "proved")
    res = sc.pass1_gate("x00A01")
    assert res["outcome"] == "defective"
    assert "build-failed" in res["reasons"]


def test_missing_artifact(repo, monkeypatch):
    assert sc.pass1_gate("ghost")["outcome"] == "missing"


def test_bounced_dispatch_stays_retryable(repo, monkeypatch):
    """The 33 problems lost to HTTP 429 must not be excluded forever."""

    monkeypatch.setattr(sc, "pass1_dispatches", lambda: [("x00A01", "j-dead")])
    monkeypatch.setattr(sc.agency, "poll_fn", lambda j: {"status": "failed"})
    assert "x00A01" not in sc.pass1_attempted()


def test_live_dispatch_holds_the_claim(repo, monkeypatch):
    """Two lanes share one approved pool; an in-flight job must block it."""

    monkeypatch.setattr(sc, "pass1_dispatches", lambda: [("x00A01", "j-live")])
    monkeypatch.setattr(sc.agency, "poll_fn", lambda j: {"status": "running"})
    assert "x00A01" in sc.pass1_attempted()


def test_quota_open_returns_zero_wait(monkeypatch):
    monkeypatch.setattr(sc.agency, "api_key", lambda: "k")
    monkeypatch.setattr(sc.agency, "_response", lambda *a, **k: (200, {}))
    monkeypatch.setattr(sc.agency, "quota_snapshot", lambda body: [
        {"unit": 3, "used": 4.0, "available": 96.0, "next_reset_ms": 0}])
    assert sc.quota_wait_seconds(min_available=0.0) == 0.0


def test_exhausted_quota_waits_for_the_reset(monkeypatch):
    import time

    reset_ms = (time.time() + 1800) * 1000
    monkeypatch.setattr(sc.agency, "api_key", lambda: "k")
    monkeypatch.setattr(sc.agency, "_response", lambda *a, **k: (200, {}))
    monkeypatch.setattr(sc.agency, "quota_snapshot", lambda body: [
        {"unit": 3, "used": 100.0, "available": 0.0, "next_reset_ms": reset_ms}])
    wait = sc.quota_wait_seconds(min_available=0.0)
    assert 1700 < wait < 1900


def test_fifty_percent_floor_no_longer_stops_a_half_used_window(monkeypatch):
    """Joe, 2026-08-06: run to exhaustion, not to a 50%-available floor."""

    monkeypatch.setattr(sc.agency, "api_key", lambda: "k")
    monkeypatch.setattr(sc.agency, "_response", lambda *a, **k: (200, {}))
    monkeypatch.setattr(sc.agency, "quota_snapshot", lambda body: [
        {"unit": 6, "used": 53.0, "available": 47.0, "next_reset_ms": 0}])
    assert sc.quota_wait_seconds(min_available=0.0) == 0.0
    assert sc.quota_wait_seconds(min_available=50.0) > 0.0


def test_unknown_job_does_not_hold_a_claim(repo, monkeypatch):
    """Zone's Agency has never seen the laptop's job-ids; nor has a restarted JVM."""

    monkeypatch.setattr(sc, "pass1_dispatches", lambda: [("x00A01", "j-gone")])

    def boom(job_id):
        raise sc.agency.AgencyError("poll failed status=404")

    monkeypatch.setattr(sc.agency, "poll_fn", boom)
    assert "x00A01" not in sc.pass1_attempted()


def test_annulled_outcome_returns_to_the_pool(repo, monkeypatch):
    """Review resetting a bad outcome to 'approved' must beat the stale commit."""

    sc.append_jsonl(sc.MANIFEST, {"at": "t", "problem-id": "x00A01",
                                  "status": "skipped", "statement-hash": None})
    monkeypatch.setattr(sc, "pass1_dispatches", lambda: [])
    monkeypatch.setattr(sc, "subprocess", None, raising=False)
    import subprocess as sp
    monkeypatch.setattr(sp, "run", lambda *a, **k: type(
        "R", (), {"stdout": "x00A01: pass-1 skipped\n"})())
    assert "x00A01" in sc.pass1_attempted()          # stale commit holds it
    sc.append_jsonl(sc.MANIFEST, {"at": "t2", "problem-id": "x00A01",
                                  "status": "approved", "reviewer": "claude-3"})
    assert "x00A01" not in sc.pass1_attempted()      # annulment frees it


def test_annulment_cannot_free_an_in_flight_job(repo, monkeypatch):
    """An in-flight job also reads 'approved'; it must keep its claim."""

    sc.append_jsonl(sc.MANIFEST, {"at": "t", "problem-id": "x00A01",
                                  "status": "approved", "statement-hash": None})
    monkeypatch.setattr(sc, "pass1_dispatches", lambda: [("x00A01", "j-live")])
    monkeypatch.setattr(sc.agency, "poll_fn", lambda j: {"status": "running"})
    assert "x00A01" in sc.pass1_attempted()


def test_detecting_a_substitution_must_not_overwrite_the_contract(repo, monkeypatch):
    """a97J07, 2026-08-06: the pass-1 gate recorded the hash it OBSERVED, so
    catching a substitution replaced the reference the substitution violated —
    re-checking then reported clean. A proving outcome must never re-freeze."""

    frozen = _freeze(repo)
    # a prover substitutes the statement; the pass-1 gate records what it saw
    sc.append_jsonl(sc.MANIFEST, {"at": "t2", "problem-id": "x00A01",
                                  "status": "void-statement-changed",
                                  "observed-statement-hash": "sha256:substituted",
                                  "gate": "pass1"})
    assert sc.frozen_hash("x00A01") == frozen

    # even a legacy record that used the authoritative key must not win
    sc.append_jsonl(sc.MANIFEST, {"at": "t3", "problem-id": "x00A01",
                                  "status": "proved",
                                  "statement-hash": "sha256:substituted"})
    assert sc.frozen_hash("x00A01") == frozen


def test_a_repair_refreezes_only_through_the_statement_gate(repo, monkeypatch):
    _freeze(repo)
    sc.append_jsonl(sc.MANIFEST, {"at": "t2", "problem-id": "x00A01",
                                  "status": "pending-review",
                                  "statement-hash": "sha256:repaired",
                                  "gate": "repair"})
    assert sc.frozen_hash("x00A01") == "sha256:repaired"


def test_cancelled_dispatch_releases_its_claim(repo, monkeypatch):
    """Trimming an over-large dispatch must return those problems to the pool."""

    monkeypatch.setattr(sc, "pass1_dispatches", lambda: [("x00A01", "j-cancelled")])
    monkeypatch.setattr(sc.agency, "poll_fn", lambda j: {"status": "cancelled"})
    assert "x00A01" not in sc.pass1_attempted()
