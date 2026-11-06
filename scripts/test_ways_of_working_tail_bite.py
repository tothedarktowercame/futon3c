"""tail-bite (eat-your-own-tail) probe tests — M-ways-of-working.

The bad cases are the point (Joe's habit rule, 2026-09-19): each constructs
the exact failure the clause is named for and shows the probe catch it.
"""
import importlib.util
import pathlib
import sys
from datetime import datetime, timedelta, timezone

spec = importlib.util.spec_from_file_location(
    "wow", pathlib.Path(__file__).parent / "ways-of-working-probe.py")
wow = importlib.util.module_from_spec(spec)
spec.loader.exec_module(wow)

NOW = datetime(2026, 11, 6, 20, 0, 0, tzinfo=timezone.utc)


def esc(s):
    return s.replace("\\", "\\\\").replace('"', '\\"')


def job(job_id, started, events, texts, summary):
    ev = " ".join(f'{{:seq {i+1}, :type "{t}", :at "{a}", :message nil}}'
                  for i, (t, a) in enumerate(events))
    te = " ".join(f'{{:seq 90{i}, :type "text", :at "{a}", :text "{esc(x)}"}}'
                  for i, (a, x) in enumerate(texts))
    return ('{:bellback-of nil, :caller "zai-9", :job-id "%s", :started-at "%s", '
            ':events [%s %s], :state "done", :result-summary "%s"}\n'
            % (job_id, started, ev, te, esc(summary)))


GOOD_SUMMARY = "Done: probe registered; warrant test-registry-29c35265ab12cd committed as 524cbd742."
NO_CITE_SUMMARY = "Done: all tests passed, the build looks good."

RUN_LO = "2026-11-06T18:00:00.000000000Z"
RUN_HI = "2026-11-06T18:05:00.000000000Z"
AFTER = "2026-11-06T19:30:00.000000000Z"
START = "2026-11-06T17:59:00.000000000Z"


def ledger(*jobs):
    return "{:version 1, :job-order [], :jobs [" + "".join(jobs) + "]}"


def test_bad_case_cold_observation_is_caught():
    """Clause 1's named bad case: the only observation postdates the run."""
    text = ledger(job("cold-1", START,
                      [("running", RUN_LO), ("done", RUN_HI)],
                      [(AFTER, "looked at the logs afterwards")],
                      GOOD_SUMMARY))
    pop, rows = wow.classify_tail_bites(wow.parse_work_jobs(text), now=NOW)
    assert pop == 1 and len(rows) == 1
    assert rows[0]["job"] == "cold-1"
    assert "no-香-during-化" in rows[0]["violation"]


def test_bad_case_uncitable_summary_is_caught():
    """Clause 2's named bad case: observed live, but nothing durable to cite."""
    text = ledger(job("nocite-1", START,
                      [("running", RUN_LO), ("done", RUN_HI)],
                      [(RUN_LO.replace("18:00", "18:02"), "reading the timer log now")],
                      NO_CITE_SUMMARY))
    pop, rows = wow.classify_tail_bites(wow.parse_work_jobs(text), now=NOW)
    assert pop == 1 and len(rows) == 1
    assert rows[0]["job"] == "nocite-1"
    assert "no-citable-既" in rows[0]["violation"]


def test_good_job_holds():
    text = ledger(job("good-1", START,
                      [("running", RUN_LO), ("done", RUN_HI)],
                      [(RUN_LO.replace("18:00", "18:01"), "timer log shows disk-watch stopped")],
                      GOOD_SUMMARY))
    pop, rows = wow.classify_tail_bites(wow.parse_work_jobs(text), now=NOW)
    assert pop == 1 and rows == []


def test_boundary_observation_at_terminal_event_counts():
    """An observation at the exact terminal instant is still during the run."""
    text = ledger(job("edge-1", START,
                      [("running", RUN_LO), ("done", RUN_HI)],
                      [(RUN_HI, "final read before close")],
                      GOOD_SUMMARY))
    _pop, rows = wow.classify_tail_bites(wow.parse_work_jobs(text), now=NOW)
    assert rows == []


def test_window_excludes_old_jobs():
    text = ledger(job("old-1", "2026-10-01T00:00:00.000000000Z",
                      [("running", "2026-10-01T00:01:00.000000000Z")],
                      [], GOOD_SUMMARY))
    pop, rows = wow.classify_tail_bites(wow.parse_work_jobs(text), now=NOW)
    assert pop == 0 and rows == []


def test_reply_jobs_are_not_population():
    """Bellback replies are observations, not interventions; they must be
    excluded (a reply chunk never reaches parse_work_jobs)."""
    reply = ('{:bellback-of "invoke-1", :caller "zai-9", :job-id "reply-1", '
             ':started-at "%s", :events [], :result-summary "%s"}\n'
             % (START, esc(NO_CITE_SUMMARY)))
    jobs = wow.parse_work_jobs(ledger(reply))
    assert jobs == []


def test_http_caller_polls_are_not_population():
    poll = ('{:bellback-of nil, :caller "http-caller", :job-id "poll-1", '
            ':started-at "%s", :events [{:seq 1, :type "accepted", :at "%s"}], '
            ':result-summary "%s"}\n' % (START, START, esc(NO_CITE_SUMMARY)))
    jobs = wow.parse_work_jobs(ledger(poll))
    assert jobs == []  # no running lifecycle


def test_probe_reports_failing_when_ledger_has_bad_rows(tmp_path):
    bad = ledger(job("cold-1", START, [("running", RUN_LO), ("done", RUN_HI)],
                     [(AFTER, "afterthought")], NO_CITE_SUMMARY))
    p = tmp_path / "jobs.edn"
    p.write_text(bad)
    live = wow.probe_tail_bite(now=NOW, ledger=str(p))
    assert live["monitor"] == "on"
    assert live["invariant"] == "failing"
    assert live["violation-count"] == 1


def test_probe_broken_when_ledger_missing(tmp_path):
    live = wow.probe_tail_bite(now=NOW, ledger=str(tmp_path / "nope.edn"))
    assert live["monitor"] == "broken"


def test_undatable_jobs_are_skipped_not_excused_into_holding():
    """No started-at means we cannot bound the turn; the job is out of the
    population (counted in parse, dropped in classify) — the probe must not
    treat silence as holding."""
    chunk = job("nodate-1", START, [("running", RUN_LO)], [], NO_CITE_SUMMARY)
    chunk = chunk.replace(':started-at "%s"' % START, ':started-at nil')
    pop, rows = wow.classify_tail_bites(wow.parse_work_jobs(ledger(chunk)), now=NOW)
    assert pop == 0 and rows == []
