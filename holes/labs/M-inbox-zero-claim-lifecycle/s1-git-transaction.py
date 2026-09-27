#!/usr/bin/env python3
"""S1 — ref/index gap closure + pinned journal recovery identities.

Imports S0 helpers (s0-git-transaction.py, same directory). Disposable repos
only; timeout + cleanup; no production/runtime/settings/network writes.

Crash tests are SIMULATED (early returns at named cutpoints). They assert
application-level ordering guarantees only. No SIGKILL, no power-failure
durability is claimed; every journal write is file+dir fsync'd, and the
durability boundary is stated in the report, not asserted beyond what ran.

Lock model: .git/index.lock with verifiable JSON ownership content, held ONLY
across the dangerous interval (immediately before ref CAS until after the
index refresh). Native git honors it (asserted); git plumbing with
GIT_INDEX_FILE locks <tmp>.lock instead, so the holder can still operate.
Foreign (non-ours, e.g. empty native) locks are never removed.
"""
import importlib.util, os, sys, json, hashlib, socket, threading, time, shutil

_here = os.path.dirname(os.path.abspath(__file__))
_spec = importlib.util.spec_from_file_location(
    "s0", os.path.join(_here, "s0-git-transaction.py"))
s0 = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(s0)
sh, out, mkrepo, write, read = s0.sh, s0.out, s0.mkrepo, s0.write, s0.read
index_entry, status, trailer_scan = s0.index_entry, s0.status, s0.trailer_scan

RESULTS = []
def rec(section, name, kind, ok, detail=""):
    RESULTS.append((section, name, kind, ok, detail))
    tag = ("DEFECT REPRODUCED" if kind == "defect" else "BEHAVIOR OK") if ok \
        else ("DEFECT NOT REPRODUCED" if kind == "defect" else "BEHAVIOR FAILED")
    print(f"ASSERT [{section}] {name}: {tag} {detail}")

def cleanup():
    s0.cleanup()

# ------------------------------------------------------------- journal
J = "witness"

def _fsync_dir(d):
    fd = os.open(d, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)

def jdir(repo):
    d = os.path.join(repo, J)
    os.makedirs(d, exist_ok=True)
    return d

def jacquire(repo, name, record):
    """Claim-targeted atomic acquisition: O_EXCL create-new wins, existing
    loses (rename-overwrite would not be single-winner). Write-once, file
    fsync + dir fsync. Returns ('won', sha256) or ('lost', record|'malformed')."""
    d = jdir(repo)
    blob = json.dumps(record, sort_keys=True).encode()
    h = hashlib.sha256(blob).hexdigest()
    p = os.path.join(d, name)
    try:
        fd = os.open(p, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
    except FileExistsError:
        try:
            with open(p, "rb") as f:
                return ("lost", json.loads(f.read().decode()))
        except Exception:
            return ("lost", "malformed")
    with os.fdopen(fd, "wb") as f:
        f.write(blob)
        f.flush()
        os.fsync(f.fileno())
    _fsync_dir(d)
    return ("won", h)

def jread(repo, name):
    p = os.path.join(repo, J, name)
    if not os.path.exists(p):
        return None
    try:
        with open(p, "rb") as f:
            return json.loads(f.read().decode())
    except Exception:
        return "malformed"

# ------------------------------------------------------------- lock
LOCK_MAGIC = "s1-index-lock"

def lock_acquire_ours(repo, tx):
    recd = {"magic": LOCK_MAGIC, "tx": tx, "pid": os.getpid(),
            "host": socket.gethostname(), "ts": time.time()}
    fd = os.open(os.path.join(repo, ".git", "index.lock"),
                 os.O_CREAT | os.O_EXCL | os.O_WRONLY)
    with os.fdopen(fd, "w") as f:
        f.write(json.dumps(recd))
        f.flush()
        os.fsync(f.fileno())
    return recd

def lock_content(repo):
    p = os.path.join(repo, ".git", "index.lock")
    if not os.path.exists(p):
        return None
    with open(p, "rb") as f:
        raw = f.read()
    try:
        c = json.loads(raw.decode())
        if isinstance(c, dict) and c.get("magic") == LOCK_MAGIC:
            return ("ours", c)
        return ("foreign", raw)
    except Exception:
        return ("foreign", raw)   # e.g. empty native git lock

def lock_release_ours(repo, expect_tx):
    """Remove only OUR lock, re-verifying content byte-for-byte immediately
    before unlink. Returns False for absent/foreign/changed locks."""
    p = os.path.join(repo, ".git", ".git" + "index.lock") if False else \
        os.path.join(repo, ".git", "index.lock")
    c = lock_content(repo)
    if not c or c[0] != "ours" or c[1].get("tx") != expect_tx:
        return False
    with open(p, "rb") as f:
        again = f.read()
    if json.loads(again.decode()).get("tx") != expect_tx:
        return False
    os.unlink(p)
    return True

def _pid_dead(pid, host):
    if host != socket.gethostname():
        return None  # cross-host liveness not checkable; recorded limitation
    try:
        os.kill(pid, 0)
        return False
    except (ProcessLookupError, OverflowError, ValueError):
        return True
    except PermissionError:
        return False

# ------------------------------------------------------------- transaction
def find_claim_commit(repo, claim, h0, tree):
    """Locate the EXACT claim commit: trailer match AND parent == h0 AND
    tree == frozen tree. Never 'current HEAD'."""
    o = out(repo, "log", "--all",
            "--format=%H%x00%P%x00%T%x00%(trailers:key=Inbox-Zero-Claim,valueonly)")
    for line in o.splitlines():
        parts = line.split("\x00")
        if len(parts) != 4:
            continue
        csha, parents, ctree, ctrailer = parts
        if ctrailer.strip() == claim and h0 in parents.split() and ctree == tree:
            return csha
    return None

def _refresh_locked(repo, path, blob, h0blob, tx):
    """Runs with .git/index.lock held by us. Build copy, guard, rename.
    The byte-compare remains as defense against non-honoring writers."""
    gitdir = os.path.join(repo, ".git")
    real = os.path.join(gitdir, "index")
    tmp = os.path.join(gitdir, f"s1-refresh-{tx}.index")
    before = read(real)
    shutil.copyfile(real, tmp)
    renv = {"GIT_INDEX_FILE": tmp}
    o = out(repo, "ls-files", "-s", "--", path, env=renv)
    cur = tuple(o.split()[:2]) if o else None
    if cur == ("100644", h0blob):
        sh(repo, "update-index", "--cacheinfo", f"100644,{blob},{path}", env=renv)
        if read(real) == before:
            os.rename(tmp, real)
        else:
            os.remove(tmp)                          # non-honoring writer; abort
    else:
        os.remove(tmp)                              # foreign staged; preserve

def s1_commit(repo, path, claim, authorized_oid, crash_at=None, tx=None):
    """Candidate transaction. authorized_oid is the frozen mint-time identity.
    Returns (verdict, commit|None). Cutpoints are simulated early returns."""
    tx = tx or f"tx-{claim}-{os.getpid()}"
    h0 = out(repo, "rev-parse", "HEAD")
    e0 = index_entry(repo, path)
    tracked = sh(repo, "cat-file", "-e", f"HEAD:{path}", check=False).returncode == 0
    h0blob = out(repo, "rev-parse", f"HEAD:{path}") if tracked else None
    if e0 != (("100644", h0blob) if h0blob else None):
        return (":staged-elsewhere", None)          # plan-time hold
    res = jacquire(repo, f"prepare-{claim}.json",
                   {"claim": claim, "h0": h0, "path": path, "e0": e0,
                    "h0blob": h0blob, "authorized": authorized_oid, "tx": tx})
    if res[0] == "lost":
        existing = res[1]
        if existing == "malformed":
            return (":journal-malformed", None)
        if existing.get("h0") != h0:
            return (":stale-plan", None)            # recheck under acquisition
        return (":prepare-lost", None)
    if crash_at == "after-prepare":
        return (":simulated-crash", None)
    data = read(os.path.join(repo, path))           # bytes captured once
    returned = out(repo, "hash-object", "-w", "--stdin", f"--path={path}",
                   input_bytes=data)
    if returned != authorized_oid:
        return (":authorization-mismatch", None)    # typed refusal, no commit
    idx = os.path.join(repo, ".git", f"s1-{tx}.index")
    env = {"GIT_INDEX_FILE": idx}
    sh(repo, "read-tree", h0, env=env)
    sh(repo, "update-index", "--add", "--cacheinfo", f"100644,{returned},{path}",
       env=env)
    frozen_tree = out(repo, "write-tree", env=env)
    jw = jacquire(repo, f"staged-{claim}.json",
                  {"claim": claim, "tx": tx, "tree": frozen_tree})
    if jw[0] == "lost":
        os.remove(idx)
        return (":journal-conflict", None)
    lock_acquire_ours(repo, tx)                     # dangerous interval begins
    crashed = False
    try:
        if out(repo, "rev-parse", "HEAD") != h0:
            return (":head-moved", None)            # finally releases lock
        newc = out(repo, "commit-tree", frozen_tree, "-p", h0, "-m",
                   f"promote {path}\n\nInbox-Zero-Claim: {claim}")
        sh(repo, "update-ref", "HEAD", newc, h0)
        if crash_at == "in-critical-after-cas":
            crashed = True                          # orphan lock left held
            return (":simulated-crash", newc)
        jw = jacquire(repo, f"outcome-{claim}.json",
                      {"claim": claim, "tx": tx, "commit": newc,
                       "parent": h0, "tree": frozen_tree})
        if jw[0] == "lost":
            return (":journal-conflict", None)
        if crash_at == "in-critical-after-outcome":
            crashed = True                          # orphan lock left held
            return (":simulated-crash", newc)
        _refresh_locked(repo, path, returned, h0blob, tx)
    finally:
        if not crashed:
            lock_release_ours(repo, tx)
            if os.path.exists(idx):
                os.remove(idx)
    jw = jacquire(repo, f"receipt-{claim}.json",
                  {"claim": claim, "tx": tx, "commit": newc,
                   "parent": h0, "tree": frozen_tree})
    return (":committed", newc) if jw[0] == "won" else (":journal-conflict", None)

def recover_s1(repo, claim):
    """Restart read-back with pinned identities. Fails closed on
    malformed/conflicting journal; never reconstructs from current HEAD."""
    prep = jread(repo, f"prepare-{claim}.json")
    if prep is None:
        return ":no-prepare"
    if prep == "malformed":
        return ":journal-malformed"
    if jread(repo, f"receipt-{claim}.json") not in (None, "malformed"):
        return ":complete"
    staged = jread(repo, f"staged-{claim}.json")
    if staged == "malformed":
        return ":journal-malformed"
    outcome = jread(repo, f"outcome-{claim}.json")
    if outcome == "malformed":
        return ":journal-malformed"
    commit = None
    if staged:
        commit = find_claim_commit(repo, claim, prep["h0"], staged["tree"])
    if outcome and (not commit or outcome.get("commit") != commit
                    or outcome.get("parent") != prep["h0"]
                    or outcome.get("tree") != (staged or {}).get("tree")):
        return ":journal-conflict"      # outcome names an unverified commit
    if not (commit and staged):
        return ":ambiguous-fail-closed"
    if not outcome:
        jacquire(repo, f"outcome-{claim}.json",
                 {"claim": claim, "tx": staged["tx"], "commit": commit,
                  "parent": prep["h0"], "tree": staged["tree"],
                  "recovered": True})
    # Orphan-lock resolution is independent of refresh: journal is resolved
    # first; then OUR lock (verified content, dead pid) is removed; a foreign
    # lock is reported and never touched.
    action = ":refresh-skipped-head-moved"
    lock_note = None
    lk = lock_content(repo)
    if lk and lk[0] == "foreign":
        lock_note = ":foreign-lock-present"
    elif lk and lk[0] == "ours":
        dead = _pid_dead(lk[1]["pid"], lk[1]["host"])
        if dead is True:
            lock_note = (":orphan-lock-cleared"
                         if lock_release_ours(repo, lk[1]["tx"])
                         else ":orphan-lock-content-changed-left")
        elif dead is None:
            lock_note = ":orphan-lock-cross-host-left"
        else:
            lock_note = ":lock-held-by-live-owner-left"
    tracked = sh(repo, "cat-file", "-e", f"HEAD:{prep['path']}",
                 check=False).returncode == 0
    cur_head_blob = out(repo, "rev-parse", f"HEAD:{prep['path']}") if tracked else None
    if cur_head_blob == prep["authorized"] and lock_note != ":foreign-lock-present":
        lock_acquire_ours(repo, staged["tx"])
        try:
            _refresh_locked(repo, prep["path"], prep["authorized"],
                            prep["h0blob"], staged["tx"])
        finally:
            lock_release_ours(repo, staged["tx"])
        action = ":refreshed"
    jw = jacquire(repo, f"receipt-{claim}.json",
                  {"claim": claim, "commit": commit, "parent": prep["h0"],
                   "tree": staged["tree"], "refresh": action,
                   "lock": lock_note})
    return ":recovered-committed" if jw[0] == "won" else ":journal-conflict"

def promotable_s1(repo, claim):
    return jread(repo, f"prepare-{claim}.json") is None

# ============================================================== sections
def hist_defects():
    """Historical defect cases re-run unmodified from S0 (labeled [repro-*])."""
    s0.repro_a()
    s0.repro_b()
    s0.repro_d1_clobber()

def owner_gap_defect():
    """Owner repro 1 against the S0 candidate: native commit during the
    post-CAS unlocked index gap reverts the promotion. Historical defect."""
    s = "owner-gap"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    write(f"{r}/f", "H\n")
    s0.candidate_commit(r, "f", "claim:gap", crash_at="after-cas")
    p = sh(r, "commit", "-qm", "ordinary concurrent commit during index gap",
           check=False)
    rec(s, "native-commit-in-gap-reverts", "defect",
        p.returncode == 0 and out(r, "show", "HEAD:f") == "X",
        "S0 candidate: native porcelain commit of stale index X lands over H "
        "during the unlocked post-CAS gap; promotion silently reverted")

def owner_receipt_defect():
    """Owner repro 2 against S0 recover(): descendant commit present, receipt
    names the wrong (descendant) commit. Historical defect."""
    s = "owner-receipt"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    write(f"{r}/f", "H\n")
    claim_commit = s0.candidate_commit(r, "f", "claim:rc", crash_at="after-cas")
    # descendant arrives through the (S0) unlocked gap
    sh(r, "commit", "-qm", "descendant during gap", check=False)
    s0.recover(r, "claim:rc")
    receipt = s0.jread(r, "receipt-claim:rc.json") or {}
    outcome = s0.jread(r, "outcome-claim:rc.json") or {}
    named = (receipt.get("commit") or outcome.get("commit") or "")
    rec(s, "s0-recovery-names-wrong-commit", "defect",
        named != claim_commit and named == out(r, "rev-parse", "HEAD"),
        "S0 recover() pinned the receipt to current HEAD (the descendant), "
        "not the claim commit; identity reconstruction was unverified")

def s1_ordinary():
    s = "s1-ordinary"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    write(f"{r}/f", "H\n")
    auth = out(r, "hash-object", "--stdin", "--path=f", input_bytes=b"H\n")
    v, c = s1_commit(r, "f", "claim:o", auth)
    rec(s, "commit-succeeds", "behavior", v == ":committed" and c,
        f"verdict {v}")
    rec(s, "status-clean-no-phantom", "behavior",
        status(r, "f") == "" and out(r, "diff", "--cached", "--name-only") == "",
        "ordinary success is clean: index advanced to committed blob")
    p = sh(r, "add", "f", check=False)
    rec(s, "native-usable-after", "behavior", p.returncode == 0,
        "native git writers usable immediately after completion")
    sh(r, "commit", "--allow-empty", "-qm", "later porcelain commit")
    rec(s, "later-commit-no-revert", "behavior", out(r, "show", "HEAD:f") == "H",
        "no phantom reversal remains for a later commit to land")

def s1_gap_closed():
    s = "s1-gap"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    write(f"{r}/f", "H\n")
    auth = out(r, "hash-object", "--stdin", "--path=f", input_bytes=b"H\n")
    # drive the transaction manually to hold the critical interval open
    h0 = out(r, "rev-parse", "HEAD")
    jacquire(r, "prepare-claim:g.json",
             {"claim": "claim:g", "h0": h0, "path": "f",
              "e0": index_entry(r, "f"), "h0blob": out(r, "rev-parse", "HEAD:f"),
              "authorized": auth, "tx": "tx-g"})
    idx = os.path.join(r, ".git", "s1-tx-g.index")
    env = {"GIT_INDEX_FILE": idx}
    returned = out(r, "hash-object", "-w", "--stdin", "--path=f",
                   input_bytes=read(f"{r}/f"))
    sh(r, "read-tree", h0, env=env)
    sh(r, "update-index", "--add", "--cacheinfo", f"100644,{returned},f", env=env)
    tree = out(r, "write-tree", env=env)
    jacquire(r, "staged-claim:g.json", {"claim": "claim:g", "tx": "tx-g", "tree": tree})
    lock_acquire_ours(r, "tx-g")
    p_add = sh(r, "add", "f", check=False)
    rec(s, "native-add-refused-in-critical-interval", "behavior",
        p_add.returncode != 0 and b"index.lock" in p_add.stderr,
        "native `git add` refused by the ownership-carrying index.lock")
    newc = out(r, "commit-tree", tree, "-p", h0, "-m",
               "promote f\n\nInbox-Zero-Claim: claim:g")
    sh(r, "update-ref", "HEAD", newc, h0)
    p_commit = sh(r, "commit", "-qm", "concurrent attempt", check=False)
    rec(s, "native-commit-refused-in-critical-interval", "behavior",
        p_commit.returncode != 0 and b"index.lock" in p_commit.stderr
        and out(r, "show", "HEAD:f") == "H",
        "native `git commit` refused mid-interval; promoted content not reverted")
    jacquire(r, "outcome-claim:g.json",
             {"claim": "claim:g", "tx": "tx-g", "commit": newc,
              "parent": h0, "tree": tree})
    _refresh_locked(r, "f", returned, out(r, "rev-parse", f"{h0}:f"), "tx-g")
    lock_release_ours(r, "tx-g")
    os.remove(idx)
    jacquire(r, "receipt-claim:g.json",
             {"claim": "claim:g", "commit": newc, "parent": h0, "tree": tree})
    rec(s, "completion-clean", "behavior",
        status(r, "f") == "" and lock_content(r) is None,
        "after completion: status clean, lock released")
    p2 = sh(r, "add", "f", check=False)
    rec(s, "native-usable-after-interval", "behavior", p2.returncode == 0,
        "native writers usable after release")

def s1_foreign_staged():
    s = "s1-foreign"
    # other-path foreign staged survives the whole locked transaction
    r = mkrepo()
    write(f"{r}/f", "X\n"); write(f"{r}/g", "G\n")
    sh(r, "add", "f", "g"); sh(r, "commit", "-qm", "base")
    write(f"{r}/g", "G-staged\n")
    g_blob = out(r, "hash-object", "-w", "g")
    sh(r, "update-index", "--cacheinfo", f"100644,{g_blob},g")
    write(f"{r}/f", "H\n")
    auth = out(r, "hash-object", "--stdin", "--path=f", input_bytes=b"H\n")
    v, c = s1_commit(r, "f", "claim:fo", auth)
    rec(s, "other-path-foreign-preserved", "behavior",
        v == ":committed" and index_entry(r, "g")[1] == g_blob
        and status(r, "f") == "",
        "promoted path clean; unrelated foreign staged entry intact")
    # same-path foreign staged at plan time: held, no commit, no lock residue
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    write(f"{r}/f", "B-staged\n")
    b_blob = out(r, "hash-object", "-w", "f")
    sh(r, "update-index", "--cacheinfo", f"100644,{b_blob},f")
    write(f"{r}/f", "H\n")
    auth = out(r, "hash-object", "--stdin", "--path=f", input_bytes=b"H\n")
    v, c = s1_commit(r, "f", "claim:fs", auth)
    rec(s, "same-path-foreign-held-at-plan", "behavior",
        v == ":staged-elsewhere" and c is None
        and out(r, "rev-parse", "HEAD") == h0
        and index_entry(r, "f")[1] == b_blob and lock_content(r) is None,
        "plan-time hold; no commit, foreign staged blob untouched, no lock left")

def s1_contention():
    s = "s1-contention"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    recd = {"claim": "claim:n", "h0": h0, "path": "f"}
    winners, losers = [], []
    def worker(i):
        res = jacquire(r, "prepare-claim:n.json", dict(recd, tx=f"tx-{i}"))
        (winners if res[0] == "won" else losers).append(i)
    threads = [threading.Thread(target=worker, args=(i,)) for i in range(8)]
    for t in threads: t.start()
    for t in threads: t.join()
    rec(s, "single-winner", "behavior", len(winners) == 1 and len(losers) == 7,
        f"8 racing executors: {len(winners)} winner, {len(losers)} losers")
    # stale plan recheck under acquisition
    res = jacquire(r, "prepare-claim:n.json", dict(recd, h0="0" * 40, tx="tx-late"))
    rec(s, "stale-plan-recheck", "behavior",
        res[0] == "lost" and res[1].get("h0") == h0,
        "loser re-reads the winner's prepare and can detect its own plan is stale")
    # conflicting/malformed journal fails closed
    write(f"{r}/witness/prepare-claim:bad.json", "{not json")
    rec(s, "malformed-fails-closed", "behavior",
        recover_s1(r, "claim:bad") == ":journal-malformed"
        and not promotable_s1(r, "claim:bad"),
        "malformed prepare: recovery refuses, claim never promotable")

def s1_crash_recovery():
    s = "s1-crash"
    # C1: crash after prepare (no lock held): fail closed, never reauthorize
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    write(f"{r}/f", "H\n")
    auth = out(r, "hash-object", "--stdin", "--path=f", input_bytes=b"H\n")
    v, _ = s1_commit(r, "f", "claim:c1", auth, crash_at="after-prepare")
    rec(s, "C1-after-prepare", "behavior",
        v == ":simulated-crash" and recover_s1(r, "claim:c1") == ":ambiguous-fail-closed"
        and out(r, "rev-parse", "HEAD") == h0 and not promotable_s1(r, "claim:c1"),
        "no commit, HEAD unchanged, claim excluded pending operator")
    # C2: crash in critical interval after CAS: orphan lock + exact recovery
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    write(f"{r}/f", "H\n")
    auth = out(r, "hash-object", "--stdin", "--path=f", input_bytes=b"H\n")
    v, newc = s1_commit(r, "f", "claim:c2", auth, crash_at="in-critical-after-cas")
    lk = lock_content(r)
    rec(s, "C2-orphan-lock-carries-ownership", "behavior",
        lk is not None and lk[0] == "ours" and lk[1].get("tx"),
        "orphan lock is ours-formatted with tx/pid/host, verifiable")
    p = sh(r, "add", "f", check=False)
    rec(s, "C2-native-blocked-until-recovery", "behavior", p.returncode != 0,
        "native writer refused while orphan lock unexplained")
    # simulate owner death: our pid IS alive in-test, so recovery path must
    # treat live-owner locks as not-yet-recoverable; emulate death by
    # rewriting the lock with a dead pid (content stays ours-formatted)
    dead = dict(lk[1]); dead["pid"] = 99999999
    write(f"{r}/.git/index.lock", json.dumps(dead))
    res = recover_s1(r, "claim:c2")
    receipt = jread(r, "receipt-claim:c2.json")
    rec(s, "C2-recovery-pins-exact-commit", "behavior",
        res == ":recovered-committed" and receipt
        and receipt.get("commit") == newc and lock_content(r) is None
        and status(r, "f") == "",
        "recovery verified trailer+parent+tree, pinned exact commit, cleared "
        "dead-owner lock, refreshed index; status clean")
    p = sh(r, "add", "f", check=False)
    rec(s, "C2-native-usable-after-recovery", "behavior", p.returncode == 0,
        "native writers usable after recovery")
    # C3: foreign (empty native) lock is never removed
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    write(f"{r}/f", "H\n")
    auth = out(r, "hash-object", "--stdin", "--path=f", input_bytes=b"H\n")
    v, newc = s1_commit(r, "f", "claim:c3", auth, crash_at="in-critical-after-cas")
    write(f"{r}/.git/index.lock", "")          # native-style empty lock
    res = recover_s1(r, "claim:c3")
    receipt = jread(r, "receipt-claim:c3.json")
    rec(s, "C3-foreign-lock-never-removed", "behavior",
        lock_content(r) is not None and lock_content(r)[0] == "foreign"
        and receipt and receipt.get("lock") == ":foreign-lock-present",
        "unexplained native lock reported and left in place; journal still "
        "resolved to the exact commit; refresh deferred, not forced")
    # C4: descendant on changed HEAD — receipt pins claim commit, refresh skipped
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    write(f"{r}/f", "H\n")
    auth = out(r, "hash-object", "--stdin", "--path=f", input_bytes=b"H\n")
    v, newc = s1_commit(r, "f", "claim:c4", auth, crash_at="in-critical-after-cas")
    os.remove(f"{r}/.git/index.lock")          # admin removed OUR lock (documented scenario)
    write(f"{r}/f", "D\n"); sh(r, "add", "f")
    sh(r, "commit", "-qm", "descendant changes f")
    res = recover_s1(r, "claim:c4")
    receipt = jread(r, "receipt-claim:c4.json")
    rec(s, "C4-receipt-pins-claim-commit-not-head", "behavior",
        res == ":recovered-committed" and receipt
        and receipt.get("commit") == newc
        and receipt.get("commit") != out(r, "rev-parse", "HEAD")
        and receipt.get("refresh") == ":refresh-skipped-head-moved",
        "descendant present: receipt names the exact claim commit (parent+tree "
        "verified), refresh skipped on changed HEAD — no blind refresh")
    rec(s, "C4-descendant-content-untouched", "behavior",
        out(r, "show", "HEAD:f") == "D",
        "descendant's content never rewritten by old-transaction recovery")

def s1_filter_refusal():
    s = "s1-filter"
    r = mkrepo()
    cnt = f"{r}/count"
    write(cnt, "0")
    clean = f"{r}/clean.sh"
    write(clean, "#!/bin/sh\nn=$(cat \"%s\"); echo $((n+1)) > \"%s\"\n"
                 "if [ \"$n\" = 0 ]; then printf 'A\\n'; else printf 'B\\n'; fi\n"
                 % (cnt, cnt), 0o755)
    write(f"{r}/.gitattributes", "f filter=demo\n")
    sh(r, "config", "filter.demo.clean", f"sh {clean}")
    sh(r, "config", "filter.demo.required", "true")
    write(f"{r}/f", "same bytes\n")
    sh(r, "add", ".gitattributes"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    # mint-time authorized identity = first filter invocation
    authorized = out(r, "hash-object", "--stdin", "--path=f",
                     input_bytes=b"same bytes\n")
    v, c = s1_commit(r, "f", "claim:flt", authorized)
    rec(s, "stateful-filter-typed-refusal", "behavior",
        v == ":authorization-mismatch" and c is None
        and out(r, "rev-parse", "HEAD") == h0,
        "execution-time persisted OID != frozen authorized OID: typed refusal, "
        "no commit, no restaging of whatever the stateful filter returned")
    rec(s, "refusal-does-not-reauthorize", "behavior",
        not promotable_s1(r, "claim:flt"),
        "claim stays excluded after mismatch; operator resolution required")

def main():
    try:
        hist_defects()
        owner_gap_defect()
        owner_receipt_defect()
        s1_ordinary()
        s1_gap_closed()
        s1_foreign_staged()
        s1_contention()
        s1_crash_recovery()
        s1_filter_refusal()
    finally:
        cleanup()
    defects = [x for x in RESULTS if x[2] == "defect"]
    behaviors = [x for x in RESULTS if x[2] == "behavior"]
    d_ok = sum(1 for x in defects if x[3]); b_ok = sum(1 for x in behaviors if x[3])
    print(f"\nSUMMARY: defect-reproductions {d_ok}/{len(defects)} confirmed; "
          f"desired-behavior demonstrations {b_ok}/{len(behaviors)} passed")
    failed = [x for x in RESULTS if not x[3]]
    if failed:
        print("UNMET ASSERTIONS:")
        for s, n, k, _, d in failed:
            print(f"  [{s}] {n}: {d}")
        sys.exit(1)

if __name__ == "__main__":
    main()
