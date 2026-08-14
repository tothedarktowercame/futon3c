"""Tests for the pass-1 lane: outcome gating, claim verification, quota waits."""

from __future__ import annotations

import argparse
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


# --- closer eligibility (2026-08-07) --------------------------------------
# Selecting only `status == "partial"` made the closer lane a no-op once those
# hit the 3-hop cap, while 103 skipped and 51 approved artifacts with open
# sorries stayed invisible to it.

def _add(repo: Path, pid: str, *, sorries: int, status) -> None:
    lean = repo / "problems" / pid / "lean"
    lean.mkdir(parents=True, exist_ok=True)
    body = "theorem apm_%s : True := by\n" % pid.lower()
    body += "".join("  sorry\n" for _ in range(sorries)) or "  trivial\n"
    (lean / "Main.lean").write_text(body, encoding="utf-8")
    if status is not None:
        sc.append_jsonl(sc.MANIFEST, {"at": "t", "problem-id": pid,
                                      "status": status})


def test_closer_sees_skipped_and_approved_not_just_partial(repo):
    _add(repo, "x00A02", sorries=1, status="skipped")
    _add(repo, "x00A03", sorries=1, status="approved")
    _add(repo, "x00A04", sorries=1, status="partial")
    assert {p for p, _ in sc.closer_targets()} >= {"x00A02", "x00A03", "x00A04"}


def test_closer_never_touches_an_unreviewed_or_bad_statement(repo):
    _add(repo, "x00A05", sorries=1, status="pending-review")
    _add(repo, "x00A06", sorries=1, status="flagged")
    _add(repo, "x00A07", sorries=1, status="void-statement-changed")
    got = {p for p, _ in sc.closer_targets()}
    assert got.isdisjoint({"x00A05", "x00A06", "x00A07"})


def test_closer_skips_artifacts_with_nothing_left_to_close(repo):
    _add(repo, "x00A08", sorries=0, status="approved")
    assert "x00A08" not in {p for p, _ in sc.closer_targets()}


def test_closer_orders_reviewed_and_easy_work_first(repo):
    """The budget is finite, so a reviewed one-sorry artifact must outrank a
    hop-exhausted partial: only a close against a reviewed statement counts."""

    _add(repo, "x00A09", sorries=4, status="partial")
    _add(repo, "x00A10", sorries=1, status="approved")
    order = [p for p, _ in sc.closer_targets()]
    assert order.index("x00A10") < order.index("x00A09")


def test_closer_respects_the_hop_cap(repo):
    _add(repo, "x00A11", sorries=1, status="approved")
    for _ in range(sc.MAX_CLOSER_HOPS):
        sc.append_jsonl(sc.LEDGER, {"at": "t", "transition": "closer-hop",
                                    "problem-id": "x00A11", "problems": ["x00A11"]})
    assert "x00A11" not in {p for p, _ in sc.closer_targets()}


def test_hop_one_packet_does_not_claim_a_prior_hop(repo):
    """189 of the widened targets are at hop 1; the old template asserted a
    prior hop's boundary note existed, which the recipient can see is false."""

    _add(repo, "x00A12", sorries=1, status="skipped")
    framing = sc.closer_framing("x00A12", 0)
    assert "FIRST" in framing
    assert "prior hop's exact state" not in framing


def test_later_hop_packet_says_what_came_before(repo):
    _add(repo, "x00A13", sorries=1, status="partial")
    framing = sc.closer_framing("x00A13", 2)
    assert "hop 3" in framing and "BEYOND" in framing


# --- hop-3 escalation (2026-08-07) ----------------------------------------
# A problem that exhausts MAX_CLOSER_HOPS used to vanish from closer_targets
# with no record anywhere — the silent exhaustion that made the lane a no-op.

def _hop(repo: Path, pid: str, n: int) -> None:
    for _ in range(n):
        sc.append_jsonl(sc.LEDGER, {"at": "t", "transition": "closer-hop",
                                    "problem-id": pid, "problems": [pid]})


def test_exhausted_problem_leaves_the_queue_but_enters_escalation(repo, monkeypatch):
    monkeypatch.setattr(sc, "ESCALATION", repo / "escalation.jsonl")
    _add(repo, "x00B01", sorries=1, status="partial")
    _hop(repo, "x00B01", sc.MAX_CLOSER_HOPS)
    assert "x00B01" not in {p for p, _ in sc.closer_targets()}
    assert "x00B01" in {c[0] for c in sc.escalation_candidates()}


def test_escalation_ignores_closed_and_condemned_problems(repo, monkeypatch):
    monkeypatch.setattr(sc, "ESCALATION", repo / "escalation.jsonl")
    _add(repo, "x00B02", sorries=0, status="proved")     # closed on the last hop
    _add(repo, "x00B03", sorries=1, status="flagged")    # statement is bad, not the proof
    for pid in ("x00B02", "x00B03"):
        _hop(repo, pid, sc.MAX_CLOSER_HOPS)
    got = {c[0] for c in sc.escalation_candidates()}
    assert got.isdisjoint({"x00B02", "x00B03"})


def test_clusters_rank_blockers_shared_by_several_problems():
    """One problem blocked on a lemma is a problem; several blocked on the SAME
    lemma is a ConstructionTarget whose cost is already amortised."""

    cands = [("p1", 3, "needs MeasureTheory.tendsto_setIntegral and foo_bar"),
             ("p2", 3, "blocked on MeasureTheory.tendsto_setIntegral"),
             ("p3", 3, "blocked on unrelated_thing")]
    clusters = sc.escalation_clusters(cands, min_share=2)
    assert clusters and clusters[0][0] == "MeasureTheory.tendsto_setIntegral"
    assert clusters[0][1] == ["p1", "p2"]
    assert all(i != "unrelated_thing" for i, _ in clusters)


def test_clusters_drop_filenames_and_prose_abbreviations():
    """Every boundary note cites LEMMA-INDEX.md and says "a.e.", so without a
    stoplist those outrank every real blocker and the ranking is unusable."""

    cands = [("p1", 3, "see INDEX.md; holds a.e.; needs ConstructionTargets/YoungL2.lean"),
             ("p2", 3, "see INDEX.md; holds a.e.; needs ConstructionTargets/YoungL2.lean"),
             ("p3", 3, "blocked on MeasureTheory.tendsto_setIntegral"),
             ("p4", 3, "blocked on MeasureTheory.tendsto_setIntegral")]
    got = [i for i, _ in sc.escalation_clusters(cands, min_share=2)]
    assert got == ["MeasureTheory.tendsto_setIntegral"], got


# --- stale declarations (2026-08-07) --------------------------------------

def _git(repo: Path, *args) -> str:
    import subprocess
    return subprocess.run(["git", *args], cwd=repo, capture_output=True,
                          text=True).stdout


def _commit(repo: Path, pid: str, subject: str) -> None:
    import subprocess
    p = repo / "problems" / pid / "lean" / "Main.lean"
    p.write_text(p.read_text() + "\n-- touch\n", encoding="utf-8")
    subprocess.run(["git", "add", str(p)], cwd=repo, capture_output=True)
    subprocess.run(["git", "commit", "-q", "-m", subject], cwd=repo,
                   capture_output=True)


@pytest.fixture()
def gitrepo(repo, monkeypatch):
    import subprocess
    subprocess.run(["git", "init", "-q"], cwd=repo, capture_output=True)
    subprocess.run(["git", "config", "user.email", "t@t"], cwd=repo, capture_output=True)
    subprocess.run(["git", "config", "user.name", "t"], cwd=repo, capture_output=True)
    _commit(repo, "x00A01", "initial")
    return repo


def test_a_superseded_skip_declaration_is_not_reused(gitrepo):
    """The closer's own commit must win over the morning's pass-1 skip; seven
    m95* problems were logged `skipped` on 2026-08-07 after real work landed."""

    _commit(gitrepo, "x00A01", "x00A01: pass-1 skipped (codex)")
    assert sc.declared_outcome("x00A01") == "skipped"
    _commit(gitrepo, "x00A01", "x00A01: prove Green kernel bounds")
    assert sc.declared_outcome("x00A01") is None


def test_a_current_declaration_is_still_honoured(gitrepo):
    _commit(gitrepo, "x00A01", "x00A01: pass-1 partial (codex)")
    assert sc.declared_outcome("x00A01") == "partial"


def test_another_problems_commit_does_not_shadow_the_declaration(gitrepo):
    _add(gitrepo, "x00A02", sorries=1, status=None)
    _commit(gitrepo, "x00A01", "x00A01: pass-1 skipped (codex)")
    _commit(gitrepo, "x00A02", "x00A02: prove something else")
    assert sc.declared_outcome("x00A01") == "skipped"


def test_a_flag_raised_mid_run_stops_the_next_dispatch(repo, monkeypatch):
    """t00J04 was flagged at 19:04 and a hop was still spent proving it at
    20:56 — the worklist was built once, hours earlier."""

    _add(repo, "x00C01", sorries=1, status="approved")
    assert "x00C01" in {p for p, _ in sc.closer_targets()}
    sc.append_jsonl(sc.MANIFEST, {"at": "t", "problem-id": "x00C01",
                                  "status": "flagged"})
    assert sc.manifest_state()["x00C01"] not in sc.CLOSER_ELIGIBLE


# --- repair queue vs the flag format (2026-08-08) --------------------------
# Normalising "flagged:<essay>" to status "flagged" + note emptied the repair
# queue silently: 160 restarts logging "nothing to repair" with 20 flags open.

def test_repair_queue_sees_the_normalised_flag_format(repo):
    _add(repo, "x00D01", sorries=1, status=None)
    sc.append_jsonl(sc.MANIFEST, {"at": "t", "problem-id": "x00D01",
                                  "status": "flagged", "note": "the conclusion is a literal"})
    got = dict(sc.repair_targets())
    assert "x00D01" in got
    assert got["x00D01"] == "the conclusion is a literal"


def test_repair_queue_still_reads_the_legacy_compound_status(repo):
    _add(repo, "x00D02", sorries=1, status=None)
    sc.append_jsonl(sc.MANIFEST, {"at": "t", "problem-id": "x00D02",
                                  "status": "flagged:winding number is defined as 0"})
    got = dict(sc.repair_targets())
    assert got.get("x00D02") == "winding number is defined as 0"


def test_a_flag_with_no_reason_still_reaches_the_queue(repo):
    _add(repo, "x00D03", sorries=1, status=None)
    sc.append_jsonl(sc.MANIFEST, {"at": "t", "problem-id": "x00D03", "status": "flagged"})
    assert "x00D03" in dict(sc.repair_targets())


# --- annulled hops (2026-08-08) -------------------------------------------
# When the Codex window ran out, dispatch kept succeeding and jobs died
# seconds later; 55 hops were charged for attempts that never reached a seat.

def test_an_annulled_hop_does_not_count_against_the_cap(repo):
    _add(repo, "x00E01", sorries=1, status="approved")
    for _ in range(sc.MAX_CLOSER_HOPS):
        sc.append_jsonl(sc.LEDGER, {"at": "t", "transition": "closer-hop",
                                    "problem-id": "x00E01", "problems": ["x00E01"]})
    assert "x00E01" not in {p for p, _ in sc.closer_targets()}
    sc.append_jsonl(sc.LEDGER, {"at": "t", "transition": "closer-hop-annulled",
                                "problem-id": "x00E01", "problems": ["x00E01"],
                                "reason": "job-failed"})
    targets = dict(sc.closer_targets())
    assert targets.get("x00E01") == sc.MAX_CLOSER_HOPS - 1


def test_fully_annulled_problem_reads_as_never_attempted(repo):
    _add(repo, "x00E02", sorries=1, status="approved")
    sc.append_jsonl(sc.LEDGER, {"at": "t", "transition": "closer-hop",
                                "problem-id": "x00E02", "problems": ["x00E02"]})
    sc.append_jsonl(sc.LEDGER, {"at": "t", "transition": "closer-hop-annulled",
                                "problem-id": "x00E02", "problems": ["x00E02"]})
    assert sc.closer_hops().get("x00E02") is None
    assert dict(sc.closer_targets())["x00E02"] == 0


# --- priority queue (2026-08-08) ------------------------------------------

def test_priority_file_jumps_the_status_ranking(repo, monkeypatch):
    """A freshly promoted ConstructionTarget has to be tested against the
    problems it was built for while the evidence is fresh, not hours later."""

    monkeypatch.setattr(sc, "PRIORITY_FILE", repo / "closer-priority.txt")
    _add(repo, "x00F01", sorries=1, status="approved")   # best status rank
    _add(repo, "x00F02", sorries=4, status="partial")    # worst status rank
    order = [p for p, _ in sc.closer_targets()]
    assert order.index("x00F01") < order.index("x00F02")
    sc.PRIORITY_FILE.write_text("# promoted Surfaces module\nx00F02\n", encoding="utf-8")
    order = [p for p, _ in sc.closer_targets()]
    assert order[0] == "x00F02", order          # jumps EVERYTHING, not just its rank
    assert order.index("x00F02") < order.index("x00F01")


def test_priority_file_tolerates_comments_and_blanks(repo, monkeypatch):
    monkeypatch.setattr(sc, "PRIORITY_FILE", repo / "closer-priority.txt")
    sc.PRIORITY_FILE.write_text("\n# a comment\n  x00F03  # trailing\n\n", encoding="utf-8")
    assert sc.closer_priority() == ["x00F03"]


def test_a_gate_outcome_cannot_erase_a_reviewer_flag(repo, monkeypatch):
    """The phantom-repair incident: jobs died on quota without editing, the
    statement gate passed the UNCHANGED file and wrote pending-review over
    five flags, which then vanished from the repair queue for good."""

    monkeypatch.setattr(sc, "bundle_touched_since", lambda pid, ts: False)
    _add(repo, "x00G01", sorries=1, status=None)
    sc.append_jsonl(sc.MANIFEST, {"at": "t1", "problem-id": "x00G01", "status": "flagged",
                                  "reviewer": "claude-3", "note": "winding number is 0"})
    assert "x00G01" in dict(sc.repair_targets())
    sc.append_jsonl(sc.MANIFEST, {"at": "t2", "problem-id": "x00G01",
                                  "status": "pending-review", "gate": "repair"})
    got = dict(sc.repair_targets())
    assert "x00G01" in got, "a gate outcome erased a reviewer flag"
    assert got["x00G01"] == "winding number is 0"


def test_a_reviewer_approval_does_clear_the_flag(repo):
    _add(repo, "x00G02", sorries=1, status=None)
    sc.append_jsonl(sc.MANIFEST, {"at": "t1", "problem-id": "x00G02", "status": "flagged",
                                  "reviewer": "claude-3", "note": "bad"})
    sc.append_jsonl(sc.MANIFEST, {"at": "t2", "problem-id": "x00G02", "status": "approved",
                                  "reviewer": "claude-3", "note": "repair verified"})
    assert "x00G02" not in dict(sc.repair_targets())


def test_a_repair_that_landed_clears_the_flag_from_the_queue(repo, monkeypatch):
    """Otherwise the lane re-repairs the same artifact forever: the work is
    done and the problem is review work now, not repair work."""

    monkeypatch.setattr(sc, "bundle_touched_since", lambda pid, ts: True)
    _add(repo, "x00H01", sorries=1, status=None)
    sc.append_jsonl(sc.MANIFEST, {"at": "t1", "problem-id": "x00H01", "status": "flagged",
                                  "reviewer": "claude-3", "note": "bad"})
    assert "x00H01" in dict(sc.repair_targets())
    sc.append_jsonl(sc.MANIFEST, {"at": "t2", "problem-id": "x00H01",
                                  "status": "pending-review", "gate": "repair"})
    assert "x00H01" not in dict(sc.repair_targets())


def test_a_flag_raised_AFTER_a_repair_goes_back_in_the_queue(repo):
    _add(repo, "x00H02", sorries=1, status=None)
    sc.append_jsonl(sc.MANIFEST, {"at": "t1", "problem-id": "x00H02",
                                  "status": "pending-review", "gate": "repair"})
    sc.append_jsonl(sc.MANIFEST, {"at": "t2", "problem-id": "x00H02", "status": "flagged",
                                  "reviewer": "claude-3", "note": "still wrong"})
    assert dict(sc.repair_targets()).get("x00H02") == "still wrong"


def test_a_non_repair_gate_still_cannot_erase_a_flag(repo):
    """The phantom-job case: a closer hop or statement gate must not clear it."""

    _add(repo, "x00H03", sorries=1, status=None)
    sc.append_jsonl(sc.MANIFEST, {"at": "t1", "problem-id": "x00H03", "status": "flagged",
                                  "reviewer": "claude-3", "note": "winding number is 0"})
    sc.append_jsonl(sc.MANIFEST, {"at": "t2", "problem-id": "x00H03",
                                  "status": "partial", "gate": "closer-hop-2"})
    assert "x00H03" in dict(sc.repair_targets())


def test_an_unreviewed_repaired_statement_is_not_closer_eligible(repo):
    """b97J01 went flagged -> pending-review (repair) -> partial (pass1-gate)
    in ten minutes; that last gate row would have handed an unreviewed
    statement back to the closer."""

    _add(repo, "x00J01", sorries=1, status=None)
    sc.append_jsonl(sc.MANIFEST, {"at": "t1", "problem-id": "x00J01",
                                  "status": "pending-review", "gate": "repair"})
    sc.append_jsonl(sc.MANIFEST, {"at": "t2", "problem-id": "x00J01",
                                  "status": "partial", "gate": "pass1"})
    assert "x00J01" not in {p for p, _ in sc.closer_targets()}


def test_a_reviewer_approval_restores_eligibility(repo):
    _add(repo, "x00J02", sorries=1, status=None)
    sc.append_jsonl(sc.MANIFEST, {"at": "t1", "problem-id": "x00J02",
                                  "status": "pending-review", "gate": "repair"})
    sc.append_jsonl(sc.MANIFEST, {"at": "t2", "problem-id": "x00J02",
                                  "status": "approved", "reviewer": "claude-3"})
    assert "x00J02" in {p for p, _ in sc.closer_targets()}


# --- gate caching (2026-08-08) --------------------------------------------
# A gate run is a full Mathlib elaboration; problems/ is not a lean_lib so
# nothing is incremental. b97J01 was re-elaborated every ten minutes for the
# same verdict because `approved` is not in the gate's `settled` set.

def test_artifact_hash_tracks_the_file(repo):
    h1 = sc.artifact_hash("x00A01")
    assert h1
    p = repo / "problems" / "x00A01" / "lean" / "Main.lean"
    p.write_text(p.read_text() + "\n-- touched\n", encoding="utf-8")
    assert sc.artifact_hash("x00A01") != h1


def test_last_gated_hash_reads_the_newest_gate_record(repo):
    assert sc.last_gated_hash("x00A01") == ""
    sc.append_jsonl(sc.LEDGER, {"at": "t1", "transition": "pass1-gate",
                                "problem-id": "x00A01", "content-hash": "aaa"})
    sc.append_jsonl(sc.LEDGER, {"at": "t2", "transition": "pass1-gate",
                                "problem-id": "x00A01", "content-hash": "bbb"})
    assert sc.last_gated_hash("x00A01") == "bbb"


def test_an_unchanged_artifact_is_not_rebuilt(repo):
    """The saving: same bytes, same verdict, so skip the elaboration."""

    sc.append_jsonl(sc.LEDGER, {"at": "t1", "transition": "pass1-gate",
                                "problem-id": "x00A01",
                                "content-hash": sc.artifact_hash("x00A01")})
    assert sc.artifact_hash("x00A01") == sc.last_gated_hash("x00A01")


def test_a_changed_artifact_is_rebuilt(repo):
    sc.append_jsonl(sc.LEDGER, {"at": "t1", "transition": "pass1-gate",
                                "problem-id": "x00A01", "content-hash": "stale"})
    assert sc.artifact_hash("x00A01") != sc.last_gated_hash("x00A01")


def _write(repo, text: str, pid: str = "x00A01") -> None:
    (repo / "problems" / pid / "lean" / "Main.lean").write_text(text, encoding="utf-8")


def test_axiom_probe_targets_the_problems_own_theorem(repo, monkeypatch):
    """t97J01 named its helper `apm_t97J01_surjective_of_connected` and its main
    theorem `apm_t97j01`; the statement regex matches the helper first, so a
    clean helper would have certified the main theorem's axioms."""

    _write(repo, "theorem apm_x00A01_helper : True := trivial\n"
                 "theorem apm_x00a01 (n : Nat) : n + 0 = n := by\n  simp\n")
    _freeze(repo)
    _stub_build(monkeypatch, 0)
    _stub_declared(monkeypatch, "proved")
    probed = {}

    def _probe(source, name, **kwargs):
        probed["name"] = name
        return {"exit-code": 0, "timed-out": False, "line": "clean",
                "impure": [], "stderr-tail": ""}

    monkeypatch.setattr(sc.gates, "run_axiom_probe", _probe)
    sc.pass1_gate("x00A01")
    assert probed["name"] == "apm_x00a01"


def test_impure_axioms_downgrade_a_proved_close(repo, monkeypatch):
    _write(repo, BANK.replace("  sorry\n", "  simp\n"))
    _freeze(repo)
    _stub_build(monkeypatch, 0)
    _stub_declared(monkeypatch, "proved")
    monkeypatch.setattr(sc.gates, "run_axiom_probe", lambda *a, **k: {
        "exit-code": 0, "timed-out": False, "line": "x", "stderr-tail": "",
        "impure": ["apm_x00a01._native.native_decide.ax_1_4"]})
    res = sc.pass1_gate("x00A01")
    assert res["outcome"] == "defective"
    assert any("impure-axioms" in reason for reason in res["reasons"])


def test_a_failed_probe_does_not_void_a_close(repo, monkeypatch):
    """A flaky or timed-out elaboration must not silently retract real work."""

    _write(repo, BANK.replace("  sorry\n", "  simp\n"))
    _freeze(repo)
    _stub_build(monkeypatch, 0)
    _stub_declared(monkeypatch, "proved")
    monkeypatch.setattr(sc.gates, "run_axiom_probe", lambda *a, **k: {
        "exit-code": 124, "timed-out": True, "line": None, "impure": [],
        "stderr-tail": ""})
    res = sc.pass1_gate("x00A01")
    assert res["outcome"] == "proved"
    assert "axiom-probe-failed" in res["reasons"]


def test_approving_refreezes_the_reviewed_claim_set(repo, monkeypatch):
    """A repair may replace a file's declarations. Approving without re-freezing
    leaves the contract naming the OLD claims, so every later hop is voided
    `void-statement-changed` — a01J04 was closed at hop 1 and discarded that way."""

    _write(repo, "theorem old_helper : True := trivial\n")
    _freeze(repo)
    sc.append_jsonl(sc.MANIFEST, {"at": "t", "problem-id": "x00A01",
                                  "status": "approved",
                                  "declaration-hashes": sc.gates.declaration_hashes(
                                      "theorem old_helper : True := trivial\n")})
    repaired = "theorem apm_x00a01 (n : Nat) : n + 0 = n := by\n  simp\n"
    _write(repo, repaired)
    assert sc.gates.declaration_set_drift(sc.frozen_declarations("x00A01"), repaired)

    args = argparse.Namespace(problem="x00A01", approve=True, reason=None,
                              reviewer="claude-3")
    sc.cmd_review(args)
    assert sc.gates.declaration_set_drift(sc.frozen_declarations("x00A01"), repaired) == []


def test_flagging_keeps_the_reason_out_of_the_status(repo):
    _freeze(repo)
    args = argparse.Namespace(problem="x00A01", approve=False,
                              reason="fabricated constant", reviewer="claude-3")
    sc.cmd_review(args)
    assert sc.manifest_state()["x00A01"] == "flagged"


def test_opaque_placeholders_downgrade_a_proved_close(repo, monkeypatch):
    """t98A05 made wedge, oriented integration and the exterior derivative all
    opaque, so a zero-sorry axiom-clean build would still prove nothing."""

    _write(repo, "opaque wedge (n : Nat) : Nat\n"
                 "theorem apm_x00a01 (n : Nat) : n + 0 = n := by\n  simp\n")
    _freeze(repo)
    _stub_build(monkeypatch, 0)
    _stub_declared(monkeypatch, "proved")
    monkeypatch.setattr(sc.gates, "run_axiom_probe", lambda *a, **k: {
        "exit-code": 0, "timed-out": False, "line": "clean", "impure": [],
        "stderr-tail": ""})
    res = sc.pass1_gate("x00A01")
    assert res["outcome"] == "defective"
    assert any("opaque-placeholders" in reason for reason in res["reasons"])
