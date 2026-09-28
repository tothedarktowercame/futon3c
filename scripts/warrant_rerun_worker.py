#!/usr/bin/env python3
"""Run explicitly queued warrant registrations without touching Agency."""
import argparse
import concurrent.futures
import datetime
import os
from pathlib import Path
import sqlite3
import subprocess
import sys
import time

sys.path.insert(0, str(Path(__file__).resolve().parent))
import warrant_index

DEFAULT_DB = "/home/joe/code/storage/test-registry/warrant-index.sqlite"
DEFAULT_RUNNER = "/home/joe/code/futon2/scripts/wm/register-warrant.sh"
REPOS = {"futon2": "/home/joe/code/futon2", "futon3c": "/home/joe/code/futon3c"}
# Declared scope for namespaces the registration script cannot derive one for
# and that have no earlier run to take it from. The warrant's reach is the
# recorded load closure; this is only the script's required declaration.
DEFAULT_CODE_PATHS = {"futon3c.diagramprover.": "test/futon3c/diagramprover/wm_wire.clj"}

def code_paths(previous, namespace):
    """The earlier run's declared code paths, else the declared default."""
    if previous:
        declared = (warrant_index.edn(previous[4]).get("scope") or {}).get("code-paths")
        if declared:
            return " ".join(declared)
    for prefix, paths in DEFAULT_CODE_PATHS.items():
        if namespace.startswith(prefix):
            return paths
    return None
DDL = """
CREATE TABLE IF NOT EXISTS warrant_rerun_requests (
 request_id INTEGER PRIMARY KEY AUTOINCREMENT, namespace TEXT NOT NULL,
 repo TEXT NOT NULL, reason TEXT NOT NULL CHECK(reason IN ('stale','absent')),
 requested_at TEXT NOT NULL, state TEXT NOT NULL CHECK(state IN ('queued','running','done','failed')),
 entry_id TEXT, detail TEXT, finished_at TEXT);
CREATE UNIQUE INDEX IF NOT EXISTS warrant_rerun_one_active_namespace
 ON warrant_rerun_requests(namespace) WHERE state IN ('queued','running');
"""

def connect(path):
    db = sqlite3.connect(path, timeout=30)
    db.executescript(DDL)
    return db

def clean_git_env():
    env = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR"): env.pop(key, None)
    return env

def latest(db, namespace):
    return db.execute("""SELECT r.entry_id,r.repo_root,r.warrant,r.revision,e.payload_text,
      r.ran_order FROM registry_runs r JOIN registry_entries e ON e.id=r.entry_id
      WHERE r.namespace=? ORDER BY r.ran_order DESC,r.finished_order DESC,r.entry_id DESC LIMIT 1""",
                      (namespace,)).fetchone()

def claim(db, limit, excluded=()):
    db.execute("BEGIN IMMEDIATE")
    sql = "SELECT request_id FROM warrant_rerun_requests WHERE state='queued'"
    args = []
    if excluded:
        sql += " AND request_id NOT IN (%s)" % ",".join("?" * len(excluded)); args += list(excluded)
    ids = [r[0] for r in db.execute(sql + " ORDER BY request_id LIMIT ?", (*args, limit))]
    if ids:
        db.execute("UPDATE warrant_rerun_requests SET state='running',detail=NULL WHERE request_id IN (%s)"
                   % ",".join("?" * len(ids)), ids)
    db.commit()
    return ids

def test_file(namespace):
    return "test/" + namespace.replace("-", "_").replace(".", "/") + ".clj"

def repo_of(path):
    """The git checkout that holds PATH, or None."""
    directory = os.path.dirname(path)
    while directory and directory != "/":
        if os.path.exists(os.path.join(directory, ".git")):
            return directory
        directory = os.path.dirname(directory)
    return None

def dirty_path(root, previous, namespace):
    """First modified tracked file among the recorded files, in whichever
    checkout holds it. A test's recorded files span several repositories."""
    if previous:
        payload = warrant_index.edn(previous[4])
        paths = list(warrant_index.recorded_files(
            {"repo-root": previous[1]}, payload, root).keys())
    else:
        paths = [os.path.join(root, test_file(namespace))]
    by_repo = {}
    for path in paths:
        repo = repo_of(path)
        if repo:
            by_repo.setdefault(repo, []).append(os.path.relpath(path, repo))
    for repo, relative in sorted(by_repo.items()):
        result = subprocess.run(["git", "status", "--porcelain", "--untracked-files=no", "--", *relative],
                                cwd=repo, env=clean_git_env(), text=True, capture_output=True, check=True)
        if result.stdout:
            return os.path.join(repo, result.stdout.splitlines()[0][3:])
    return None

def finish(db_path, request_id, state, entry=None, detail=None):
    with connect(db_path) as db:
        db.execute("UPDATE warrant_rerun_requests SET state=?,entry_id=?,detail=?,finished_at=? WHERE request_id=?",
                   (state, entry, detail, datetime.datetime.now(datetime.timezone.utc).isoformat(), request_id))

def process_one(db_path, request_id, runner, log_dir):
    with connect(db_path) as db:
        row = db.execute("SELECT namespace,repo,requested_at FROM warrant_rerun_requests WHERE request_id=?",
                         (request_id,)).fetchone()
        namespace, repo, requested = row
        root = REPOS.get(repo)
        if not root:
            finish(db_path, request_id, "failed", detail="unknown repo: " + repo); return
        previous = latest(db, namespace)
    head = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root,
                                   env=clean_git_env(), text=True).strip()
    dirty = dirty_path(root, previous, namespace)
    if dirty:
        with connect(db_path) as db:
            db.execute("UPDATE warrant_rerun_requests SET state='queued',detail=? WHERE request_id=?",
                       ("held: dirty " + dirty, request_id))
        return
    if subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root,
                               env=clean_git_env(), text=True).strip() != head:
        with connect(db_path) as db:
            db.execute("UPDATE warrant_rerun_requests SET state='queued',detail=? WHERE request_id=?",
                       ("held: HEAD moved", request_id))
        return
    log = Path(log_dir) / f"{request_id}-{namespace}.log"; log.parent.mkdir(parents=True, exist_ok=True)
    env = clean_git_env()
    env.update(AUTHOR="warrant-rerun-worker", REGISTRY_DB=db_path,
               WARRANT_WORKTREE_SUFFIX=f"rerun-{request_id}")
    declared = code_paths(previous, namespace)
    if declared:
        env["CODE_PATHS"] = declared
    try:
        with log.open("wb") as out:
            code = subprocess.run([runner, "--pinned", head, namespace], cwd=root, env=env,
                                  stdout=out, stderr=subprocess.STDOUT, timeout=900).returncode
    except subprocess.TimeoutExpired:
        code = 124
    with connect(db_path) as db: after = latest(db, namespace)
    # A run is new when its entry differs from the one seen before the child
    # started. (ran_order is an epoch key, requested_at an ISO time; the two
    # do not compare.)
    entry = after[0] if after and (previous is None or after[0] != previous[0]) else None
    if entry and after[2] and after[3] == head:
        finish(db_path, request_id, "done", entry=entry)
    elif entry and not after[2] and ":scope-not-committed" in (after[4] or ""):
        # The registration refused because a file the test loads was edited
        # while it ran. The test result says nothing either way; the request
        # goes back to the queue and is not tried again in this pass.
        with connect(db_path) as db:
            db.execute("UPDATE warrant_rerun_requests SET state='queued',entry_id=?,detail=? WHERE request_id=?",
                       (entry, "held: a loaded file was edited during the run (scope-not-committed)", request_id))
    else:
        last = ""
        try:
            lines = [l for l in log.read_text(errors="replace").splitlines() if l.strip()]
            last = lines[-1][:200] if lines else ""
        except OSError:
            pass
        finish(db_path, request_id, "failed", entry=entry,
               detail=f"runner exit {code}: {last}" if last else f"runner exit {code}")

def guarded(db_path, request_id, runner, log_dir):
    """An error in one request fails that request; it never leaves it running."""
    try:
        process_one(db_path, request_id, runner, log_dir)
    except Exception as error:  # noqa: BLE001
        finish(db_path, request_id, "failed", detail="worker error: %r" % (error,))

def run_pass(args):
    seen = set()
    while True:
        with connect(args.db) as db: ids = claim(db, args.parallel, seen)
        if not ids: return
        seen.update(ids)
        with concurrent.futures.ThreadPoolExecutor(max_workers=args.parallel) as pool:
            list(pool.map(lambda i: guarded(args.db, i, args.runner, args.log_dir), ids))

def main(argv=None):
    p = argparse.ArgumentParser(); p.add_argument("--db", default=DEFAULT_DB)
    p.add_argument("--parallel", type=int, default=1); p.add_argument("--runner", default=DEFAULT_RUNNER)
    p.add_argument("--log-dir", default="/tmp/warrant-rerun-logs")
    mode = p.add_mutually_exclusive_group(required=True); mode.add_argument("--once", action="store_true")
    mode.add_argument("--watch", type=float)
    args = p.parse_args(argv)
    while True:
        run_pass(args)
        if args.once: return 0
        time.sleep(args.watch)

if __name__ == "__main__": raise SystemExit(main())
