#!/usr/bin/env python3
"""Per-file pytest warrant sweep for an mfuton checkout (claude-4 requisition,
2026-09-30). Model: futon2/scripts/warrant_suite.py — one registry record per
test FILE, an index mapping file -> entry id, --dry-run, -j, named subset,
currency checks fast (serving JVM when it has the endpoint, CLI fallback in
its own JVM), runs in their own processes.

"Current" is check-currency!, NOT check-record!: a current FAILING file is
skipped and its recorded outcomes reported. No record, stale record, or
:no-load-closure (killed run) -> rerun.

Each pytest execution runs inside
  systemd-run --user --scope -q -p MemoryMax=<cap> -p MemorySwapMax=0
  timeout <seconds> /real/python -m pytest <file>
through a tiny `python` wrapper (below): the registry's logical command stays
EXACTLY [python, -m, pytest, <file>] — the wrapper IS the named interpreter
(validate-command! pins an absolute python path; its bytes are fingerprinted,
and --version/-c pass through to the real interpreter underneath).

Usage (env: MFUTON_HOME, PYTHONPATH, PATH as the suite needs them;
PYTEST_ADDOPTS=-p no:cacheprovider recommended so no cache lands in the
checkout):
  scripts/pytest_warrant_suite.py --repo R --test-dir D --python P [files...]
    [--dry-run] [-j 3] [--timeout 400] [--memory-max 12G]
    [--index PATH] [--artifacts DIR] [--author A]

Report: per-sweep counts (skipped/rerun, failing, aborted), wall time, and
the UNION of per-test outcomes (current + rerun) as one sorted
PASSED/FAILED/ERROR/ABORTED(rc) list — the /tmp/ca-*/outcomes shape, so
sweeps are diffable across runs.
"""
import argparse
import json
import os
import subprocess
import sys
import tempfile
import time
import urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

FUTON3C = Path(__file__).resolve().parent.parent
DEFAULT_ROOT = Path.home() / "code/storage/test-registry/mfuton-suite"
AGENCY = os.environ.get("AGENCY_URL", "http://localhost:7070")


def wrapper_python(real_python, memory_max, timeout_s, bin_dir):
    """A stable absolute path named `python` that execs the real interpreter
    under the systemd memory scope and timeout. The registry command names
    THIS path; its sha pins the wrapper, so changing cap/timeout stales."""
    bin_dir.mkdir(parents=True, exist_ok=True)
    w = bin_dir / "python"
    w.write_text("#!/bin/sh\nexec systemd-run --user --scope -q --collect "
                 "-p MemoryMax=%s -p MemorySwapMax=0 "
                 "timeout %d %s \"$@\"\n" % (memory_max, timeout_s, real_python))
    w.chmod(0o755)
    return w


def test_files(repo, test_dir):
    return sorted(str(p.relative_to(repo)) for p in (repo / test_dir).rglob("*_test.py"))


def code_paths(repo, wanted):
    paths = [p for p in wanted if (repo / p).is_file()]
    return paths or None


def edn_str(s):
    return '"' + str(s).replace("\\", "\\\\").replace('"', '\\"') + '"'


def edn_vec(xs):
    return "[" + " ".join(edn_str(x) for x in xs) + "]"


def registry(op, spec_edn):
    with tempfile.NamedTemporaryFile("w", suffix=".edn", delete=False) as fh:
        fh.write(spec_edn)
        path = fh.name
    try:
        p = subprocess.run(["clojure", "-M", "-m", "futon3c.test-registry", op, path],
                           cwd=FUTON3C, capture_output=True, text=True)
    finally:
        os.unlink(path)
    lines = [l for l in p.stdout.splitlines() if l.startswith("{")]
    try:
        return json.loads(lines[-1]) if lines else {"error": (p.stderr or p.stdout)[-2000:]}
    except json.JSONDecodeError:
        return {"error": lines[-1][-2000:]}


def currency_http(entry_id, repo):
    body = json.dumps({"entry-id": entry_id, "repo-root": str(repo),
                       "changed-paths": []}).encode()
    req = urllib.request.Request(AGENCY + "/api/alpha/test-registry/currency",
                                 data=body, headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=120) as r:
        out = json.loads(r.read())
    return out.get("currency", out)


def currency(entry_id, repo):
    """Try the serving JVM first (sub-second); fall back to the CLI in its own
    JVM (~30 s) when the endpoint is absent or unreachable."""
    try:
        return currency_http(entry_id, repo)
    except Exception:
        return registry("currency", "{:entry-id %s :repo-root %s :changed-paths [] :output :json}"
                        % (edn_str(entry_id), edn_str(str(repo))))


def register(repo, python, rel_file, code, artifacts, author):
    spec = ("{:repo-root %s :code-paths %s :test-paths %s :command %s "
            ":author %s :artifact-dir %s :output :json}"
            % (edn_str(str(repo)), edn_vec(code), edn_vec([rel_file]),
               edn_vec([str(python), "-m", "pytest", rel_file]), edn_str(author),
               edn_str(str(artifacts))))
    return registry("run", spec)


def outcome_lines(results, rel_file):
    """PASSED/FAILED/ERROR per-test lines, or ABORTED(rc) when the process was
    killed before pytest could report (no parseable summary)."""
    if not isinstance(results, dict):
        return []
    outcomes = results.get("outcomes")
    if isinstance(outcomes, list) and outcomes:
        return list(outcomes)
    tests = results.get("tests")
    if isinstance(tests, dict) and tests.get("reason") == "unparsed":
        exit_code = results.get("exit")
        return ["ABORTED(rc=%s) %s" % (exit_code, rel_file)]
    return []


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo", required=True, type=Path)
    ap.add_argument("--test-dir", required=True)
    ap.add_argument("--python", required=True, type=Path,
                    help="absolute path to the real interpreter (abs path required by the registry)")
    ap.add_argument("--index", type=Path, default=DEFAULT_ROOT / "index.json")
    ap.add_argument("--artifacts", type=Path, default=DEFAULT_ROOT / "artifacts")
    ap.add_argument("--bin-dir", type=Path, default=DEFAULT_ROOT / "bin")
    ap.add_argument("--author", default=os.environ.get("WARRANT_AUTHOR", "zai-3"))
    ap.add_argument("--code-paths", default="conftest.py,pytest.ini",
                    help="comma-separated declared scope paths that must exist")
    ap.add_argument("--timeout", type=int, default=400)
    ap.add_argument("--memory-max", default="12G")
    ap.add_argument("-j", type=int, default=3)
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("files", nargs="*", help="named subset (repo-relative test files)")
    a = ap.parse_args()

    repo = a.repo.resolve()
    if not a.python.is_absolute():
        sys.exit("--python must be absolute")
    python = wrapper_python(a.python, a.memory_max, a.timeout, a.bin_dir)
    all_files = test_files(repo, a.test_dir)
    files = a.files or all_files
    known = set(all_files)
    unknown = [f for f in files if f not in known]
    if unknown:
        sys.exit("not test files under %s: %s" % (a.test_dir, unknown))
    code = code_paths(repo, a.code_paths.split(","))
    if code is None:
        sys.exit("none of the declared code paths exist: %s" % a.code_paths)

    index_path = a.index
    index = json.loads(index_path.read_text()) if index_path.exists() else {}

    index_lock = __import__("threading").Lock()

    def one(rel):
        entry = index.get(rel)
        if entry and entry.get("entry-id"):
            c = currency(entry["entry-id"], repo)
            if c.get("current?") is True:
                return rel, "skipped-current", c.get("results"), entry.get("entry-id")
            if a.dry_run:
                return rel, "needs-run:" + str(c.get("reason")), None, entry.get("entry-id")
        elif a.dry_run:
            return rel, "needs-run:no-record", None, None
        stamp = time.strftime("%Y%m%dT%H%M%SZ", time.gmtime())
        result = register(repo, python, rel, code,
                          a.artifacts / (rel.replace("/", "_") + "/" + stamp), a.author)
        payload = result.get("payload") or {}
        entry_id = result.get("evidence/id") or result.get("id")
        results = payload.get("results")
        if isinstance(results, dict) and isinstance(results.get("outcomes"), list) and results["outcomes"]:
            status = "failing" if results.get("failures") or results.get("errors") else "rerun-ok"
        else:
            status = "aborted"
        return rel, status, results, entry_id

    tally = {}
    outcome_union = {}
    started = time.time()
    with ThreadPoolExecutor(max_workers=a.j) as pool:
        futures = {pool.submit(one, f): f for f in files}
        for fut in as_completed(futures):
            rel, status, results, entry_id = fut.result()
            tally[status.split(":")[0]] = tally.get(status.split(":")[0], 0) + 1
            lines = outcome_lines(results, rel)
            if lines:
                outcome_union[rel] = lines
            if not a.dry_run and entry_id:
                with index_lock:
                    index[rel] = {"entry-id": entry_id}
                    index_path.parent.mkdir(parents=True, exist_ok=True)
                    index_path.write_text(json.dumps(index, indent=1, sort_keys=True))
            print("%-16s %s %s" % (status, rel, entry_id or ""), flush=True)
            for line in lines:
                if line.startswith(("FAILED", "ERROR", "ABORTED")):
                    print("    " + line, flush=True)

    all_outcomes = sorted(l for lines in outcome_union.values() for l in lines)
    report = {"repo": str(repo), "test-dir": a.test_dir, "files": len(files),
              "counts": tally, "wall-seconds": round(time.time() - started, 1),
              "outcome-counts": {p: sum(1 for l in all_outcomes if l.startswith(p))
                                 for p in ("PASSED", "FAILED", "ERROR", "ABORTED")}}
    print(json.dumps(report), flush=True)
    outcomes_path = a.artifacts.parent / "outcomes"
    outcomes_path.parent.mkdir(parents=True, exist_ok=True)
    outcomes_path.write_text("".join(l + "\n" for l in all_outcomes))
    print("outcomes -> %s" % outcomes_path, flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
