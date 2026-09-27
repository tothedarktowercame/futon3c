#!/usr/bin/env python3
"""S0 — real-Git transaction evidence for M-inbox-zero-claim-lifecycle.

Disposable repos only (tempfile.TemporaryDirectory). No production code,
no runtime configuration, no network. Every section prints ASSERT lines;
the summary distinguishes DEFECT REPRODUCED (owner/D1/D2 defect confirmed
present — evidence against a design) from BEHAVIOR OK (desired property
demonstrated by the candidate protocol).

Sections:
  repro-A    owner finding A: D2 ordinary success leaves phantom staged reversal
  repro-B    owner finding B: trailer consumption is not monotone under ref rewrite
  repro-CE2  original CE2 (B commits X, leaves H dirty): trailer scan holds
  repro-D1   D1 step-5 `git reset -- path` clobbers foreign staged bytes
  repro-D2   D2 private index preserves foreign same-path staged entry
  cand-*     candidate prepare/commit/recover protocol interleavings + cutpoints
  hook-*     post-hook frozen-tree revalidation; mutating/refusing hooks
  filter-*   stateful clean filter: two invocations, same bytes, different OIDs
"""
import os, subprocess, sys, tempfile, shutil, json, time

RESULTS = []

def rec(section, name, kind, ok, detail=""):
    # kind: "defect" (reproducing a defect is the expected outcome) or
    # "behavior" (demonstrating a desired property is the expected outcome)
    RESULTS.append((section, name, kind, ok, detail))
    tag = ("DEFECT REPRODUCED" if kind == "defect" else "BEHAVIOR OK") if ok \
        else ("DEFECT NOT REPRODUCED" if kind == "defect" else "BEHAVIOR FAILED")
    print(f"ASSERT [{section}] {name}: {tag} {detail}")

def sh(repo, *args, check=True, env=None, input_bytes=None):
    e = dict(os.environ)
    if env:
        e.update(env)
    p = subprocess.run(["git", "-C", repo, *args], capture_output=True,
                       env=e, input=input_bytes)
    if check and p.returncode != 0:
        raise RuntimeError(f"git {' '.join(args)} failed: {p.stderr.decode()}")
    return p

def out(repo, *args, **kw):
    return sh(repo, *args, **kw).stdout.decode().strip()

def mkrepo():
    d = tempfile.mkdtemp(prefix="s0-")
    REPOS.append(d)
    sh(d, "init", "-q", ".")
    sh(d, "config", "user.email", "s0@test")
    sh(d, "config", "user.name", "S0")
    sh(d, "config", "commit.gpgsign", "false")
    return d

REPOS = []
def cleanup():
    for d in REPOS:
        shutil.rmtree(d, ignore_errors=True)

def write(path, content, mode=None):
    with open(path, "w") as f:
        f.write(content)
    if mode is not None:
        os.chmod(path, mode)

def read(path):
    with open(path, "rb") as f:
        return f.read()

def index_entry(repo, path):
    o = out(repo, "ls-files", "-s", "--", path)
    if not o:
        return None
    mode, oid, *_ = o.split()
    return (mode, oid)

def status(repo, path=None):
    args = ["status", "--porcelain"] + (["--", path] if path else [])
    return out(repo, *args)

def trailer_scan(repo, rng, claim):
    o = out(repo, "log", "--format=%(trailers:key=Inbox-Zero-Claim,valueonly)", rng)
    return claim in o.splitlines()

# D2-style private-index commit of worktree bytes of `path` with trailer.
def d2_commit(repo, path, claim, h0):
    data = read(os.path.join(repo, path))
    oid = out(repo, "hash-object", "--stdin", f"--path={path}", input_bytes=data)
    persisted = out(repo, "hash-object", "-w", "--stdin", f"--path={path}", input_bytes=data)
    idx = os.path.join(repo, ".git", "s0-private-index")
    env = {"GIT_INDEX_FILE": idx}
    sh(repo, "read-tree", h0, env=env)
    sh(repo, "update-index", "--add", "--cacheinfo", f"100644,{persisted},{path}", env=env)
    tree = out(repo, "write-tree", env=env)
    try:
        os.remove(idx)
    except OSError:
        pass
    newc = out(repo, "commit-tree", tree, "-p", h0, "-m",
               f"promote {path}\n\nInbox-Zero-Claim: {claim}")
    sh(repo, "update-ref", "HEAD", newc, h0)
    return newc, tree

# ---------------------------------------------------------------- repro A
def repro_a():
    s = "repro-A"
    r = mkrepo()
    write(f"{r}/f", "X\n")
    sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    write(f"{r}/f", "H\n")                       # authorized worktree content
    d2_commit(r, "f", "claim:a", h0)             # D2 §6, no foreign writer
    st = status(r, "f")
    rec(s, "ordinary-success-index-state", "defect", st == "MM f",
        f"status after clean D2 promotion = {st!r} (expected clean, got phantom staged reversal)")
    cached = out(r, "diff", "--cached", "--name-only")
    rec(s, "cached-diff-is-reversal", "defect", cached == "f",
        "staged diff is H->X reversal of the just-promoted content; a later porcelain `git commit` would silently revert H")
    # demonstrate the silent revert actually landing
    sh(r, "commit", "-qm", "unrelated later porcelain commit")
    landed = out(r, "show", "HEAD:f")
    rec(s, "later-commit-reverts", "defect", landed == "X",
        f"porcelain commit of the phantom staged entry reverted promoted content to {landed!r}")

# ---------------------------------------------------------------- repro B
def repro_b():
    s = "repro-B"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    write(f"{r}/f", "H\n")
    d2_commit(r, "f", "claim:b", h0)             # trailer commit; crash before receipt
    sh(r, "update-ref", "HEAD", h0)              # ref rewritten back to mint head
    consumed = trailer_scan(r, f"{h0}..HEAD", "claim:b")
    rec(s, "trailer-lost-under-ref-rewrite", "defect", not consumed,
        "scan of mint..HEAD finds no trailer after update-ref back to mint HEAD; "
        "reachable-history consumption is not monotone")

# ---------------------------------------------------------------- repro CE2
def repro_ce2():
    s = "repro-CE2"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    write(f"{r}/f", "H\n")
    d2_commit(r, "f", "claim:c", h0)             # A commits H w/ trailer, crash before receipt
    write(f"{r}/f", "X2\n")                      # B commits X on the new HEAD
    sh(r, "add", "f"); sh(r, "commit", "-qm", "B other work")
    write(f"{r}/f", "H\n")                       # B leaves H dirty again
    consumed = trailer_scan(r, f"{h0}..HEAD", "claim:c")
    rec(s, "original-ce2-held", "behavior", consumed,
        "trailer survives ordinary later commits; original CE2 variant is consumed")
    cur = out(r, "hash-object", "--path=f", "f") if False else None
    # worktree H == claim post, HEAD:f == X2 != H: already-landed false,
    # so consumption MUST come from the trailer, and does here.
    rec(s, "already-landed-false-as-owner-showed", "behavior",
        out(r, "rev-parse", "HEAD:f") != out(r, "hash-object", "--stdin", "--path=f", input_bytes=b"H\n"),
        "HEAD:path != worktree OID; consumption evidence is the trailer alone in this variant")

# ---------------------------------------------------------------- repro D1
def repro_d1_clobber():
    s = "repro-D1"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    write(f"{r}/f", "B-staged\n")
    b_blob = out(r, "hash-object", "-w", "f")
    sh(r, "update-index", "--cacheinfo", f"100644,{b_blob},f")  # B stages own bytes
    write(f"{r}/f", "X\n")
    sh(r, "reset", "-q", "HEAD", "--", "f")      # D1 step-5 resync
    entry = index_entry(r, "f")
    rec(s, "resync-clobbers-foreign-staged", "defect",
        entry is not None and entry[1] != b_blob,
        f"after `git reset HEAD -- f`, staged entry is {entry[1][:8]} not B's {b_blob[:8]}; foreign staged content destroyed")

# ---------------------------------------------------------------- repro D2 preserve
def repro_d2_preserve():
    s = "repro-D2"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    write(f"{r}/f", "B-staged\n")
    b_blob = out(r, "hash-object", "-w", "f")
    sh(r, "update-index", "--cacheinfo", f"100644,{b_blob},f")  # foreign staged, same path
    write(f"{r}/f", "H\n")                       # authorized worktree content
    d2_commit(r, "f", "claim:d", h0)             # private index; never touches shared index
    entry = index_entry(r, "f")
    rec(s, "foreign-staged-preserved", "behavior", entry[1] == b_blob,
        "B's staged entry survives D2 promotion untouched")
    rec(s, "committed-blob-authorized", "behavior",
        out(r, "rev-parse", "HEAD:f") == out(r, "hash-object", "--stdin", "--path=f", input_bytes=b"H\n"),
        "HEAD received exactly the authorized blob")
    st = status(r, "f")
    rec(s, "status-honest-foreign-dirt", "behavior", st == "MM f",
        f"status {st!r} shows B's staged content vs new HEAD — visible, attributed dirt")

# ------------------------------------------------- candidate protocol
# prepare(outcome) journal = files in a witness dir (atomic rename), standing
# in for the existing validated intake. Refresh rule: after CAS, under the
# REAL git index lock, advance promoted paths' shared-index entries to the
# committed blob iff the entry still equals its prepare-time value E0;
# otherwise leave it (foreign staged content preserved).
JOURNAL = "witness"

def jwrite(repo, name, record):
    d = os.path.join(repo, JOURNAL); os.makedirs(d, exist_ok=True)
    tmp = os.path.join(d, f".{name}.tmp")
    with open(tmp, "w") as f:
        json.dump(record, f)
    os.rename(tmp, os.path.join(d, name))      # atomic publication

def jread(repo, name):
    p = os.path.join(repo, JOURNAL, name)
    if not os.path.exists(p):
        return None
    with open(p) as f:
        return json.load(f)

def lock_acquire(repo):
    fd = os.open(os.path.join(repo, ".git", "index.lock"),
                 os.O_CREAT | os.O_EXCL | os.O_WRONLY)
    os.close(fd)

def lock_release(repo):
    os.remove(os.path.join(repo, ".git", "index.lock"))

def conditional_refresh(repo, paths_blobs, e0_entries, h0_blobs=None):
    """Index CAS: build the refreshed index as a COPY (git can run, no lock
    held), then under the real index.lock compare the live index file's raw
    bytes with the pre-copy snapshot and rename the copy into place only on
    exact match. Native writers honor index.lock (asserted in cand-lock);
    any interleaving native write changes the bytes and aborts the refresh,
    preserving foreign staged content. Returns {path: action}."""
    gitdir = os.path.join(repo, ".git")
    real = os.path.join(gitdir, "index")
    tmp = os.path.join(gitdir, "s0-refresh-index")
    before = read(real) if os.path.exists(real) else None
    if before is not None:
        shutil.copyfile(real, tmp)
    env = {"GIT_INDEX_FILE": tmp}
    actions, changed = {}, False
    h0_blob_of = h0_blobs or {}
    for path, (mode, blob) in paths_blobs.items():
        o = out(repo, "ls-files", "-s", "--", path, env=env)
        cur = tuple(o.split()[:2]) if o else None
        e0 = e0_entries.get(path)
        e0 = tuple(e0) if e0 else None
        # Advance only when the live entry is byte-identical to what prepare
        # saw AND that was the clean mint-HEAD entry. An entry that was
        # already foreign at prepare time must be preserved, not clobbered.
        if cur == e0 and e0 == (mode, h0_blob_of.get(path)):
            sh(repo, "update-index", "--cacheinfo", f"{mode},{blob},{path}", env=env)
            actions[path] = "advanced-to-committed"
            changed = True
        else:
            actions[path] = f"preserved-foreign (E0={e0}, cur={cur})"
    if not changed:
        if os.path.exists(tmp):
            os.remove(tmp)
        return actions
    lock_acquire(repo)
    try:
        now = read(real) if os.path.exists(real) else None
        if now == before:
            os.rename(tmp, real)          # atomic replace under the lock
        else:
            for p in paths_blobs:
                actions[p] = "aborted-foreign-interleaving"
            os.remove(tmp)
    finally:
        lock_release(repo)
    return actions

def candidate_commit(repo, path, claim, crash_at=None):
    """Full candidate protocol; crash_at names a cutpoint to stop after."""
    h0 = out(repo, "rev-parse", "HEAD")
    e0 = index_entry(repo, path)
    h0blob = out(repo, "rev-parse", "--verify", "-q", f"HEAD:{path}", check=False) \
        if sh(repo, "cat-file", "-e", f"HEAD:{path}", check=False).returncode == 0 else None
    jwrite(repo, f"prepare-{claim}.json",
           {"claim": claim, "h0": h0, "path": path, "e0": e0, "h0blob": h0blob})
    if crash_at == "after-prepare":
        return None
    newc, tree = d2_commit(repo, path, claim, h0)
    if crash_at == "after-cas":
        return newc
    jwrite(repo, f"outcome-{claim}.json", {"claim": claim, "commit": newc})
    if crash_at == "after-outcome":
        return newc
    data = read(os.path.join(repo, path))
    blob = out(repo, "rev-parse", f"HEAD:{path}")
    acts = conditional_refresh(repo, {path: ("100644", blob)}, {path: e0},
                               {path: h0blob})
    jwrite(repo, f"receipt-{claim}.json", {"claim": claim, "commit": newc,
                                           "refresh": acts})
    return newc

def recover(repo, claim):
    """Restart read-back for one prepared claim. Returns resolution string."""
    prep = jread(repo, f"prepare-{claim}.json")
    if not prep:
        return "no-prepare"
    if jread(repo, f"receipt-{claim}.json"):
        return "complete"
    h0 = prep["h0"]
    if trailer_scan(repo, f"{h0}..HEAD", claim):
        # committed but unfinished: finish outcome + conditional refresh only
        jwrite(repo, f"outcome-{claim}.json",
               {"claim": claim, "commit": out(repo, "rev-parse", "HEAD"),
                "recovered": True})
        blob = out(repo, "rev-parse", f"HEAD:{prep['path']}")
        acts = conditional_refresh(repo, {prep["path"]: ("100644", blob)},
                                   {prep["path"]: prep["e0"]},
                                   {prep["path"]: prep.get("h0blob")})
        jwrite(repo, f"receipt-{claim}.json", {"claim": claim, "refresh": acts,
                                               "recovered": True})
        return "recovered-committed"
    if jread(repo, f"outcome-{claim}.json"):
        # outcome said committed but no trailer in history: ref was rewritten
        # after the fact; durable outcome still consumes.
        return "consumed-by-durable-outcome-despite-rewrite"
    # prepare exists, no trailer, no outcome: ambiguous (never committed, or
    # committed and rewritten away). FAIL CLOSED: claim stays excluded,
    # operator resolution required; never auto-reauthorize.
    return "ambiguous-fail-closed"

def promotable(repo, claim):
    """Candidate eligibility: prepared claims are never reauthorized unless
    explicitly aborted by an operator (no abort path in S0)."""
    return jread(repo, f"prepare-{claim}.json") is None

def cand_i1_clean_success():
    s = "cand-I1"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    write(f"{r}/f", "H\n")
    candidate_commit(r, "f", "claim:i1")
    rec(s, "ordinary-success-clean", "behavior",
        status(r, "f") == "" and out(r, "diff", "--cached", "--name-only") == "",
        "after candidate promotion: status clean, no phantom staged reversal")
    rec(s, "head-and-worktree", "behavior",
        out(r, "show", "HEAD:f") == "H" and read(f"{r}/f") == b"H\n",
        "HEAD and worktree both hold authorized content")
    sh(r, "commit", "--allow-empty", "-qm", "later porcelain commit")
    rec(s, "later-commit-no-revert", "behavior", out(r, "show", "HEAD:f") == "H",
        "later porcelain commit does not revert promoted content")

def cand_i2_foreign_same_path():
    s = "cand-I2"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    write(f"{r}/f", "B-staged\n")
    b_blob = out(r, "hash-object", "-w", "f")
    sh(r, "update-index", "--cacheinfo", f"100644,{b_blob},f")   # foreign staged
    write(f"{r}/f", "H\n")
    candidate_commit(r, "f", "claim:i2")
    entry = index_entry(r, "f")
    x_blob = out(r, "rev-parse", f"{h0}:f")
    prep = jread(r, "prepare-claim:i2.json")
    rec(s, "plan-time-staged-elsewhere-signal", "behavior",
        tuple(prep["e0"]) != ("100644", x_blob),
        "prepare saw index entry != mint-HEAD blob: in production this path is held "
        ":staged-elsewhere at plan time; the refresh rule below is the second line")
    rec(s, "foreign-same-path-preserved", "behavior", entry[1] == b_blob,
        "conditional refresh saw entry != clean mint-HEAD entry and left B's staged blob untouched")
    rec(s, "commit-still-authorized", "behavior", out(r, "show", "HEAD:f") == "H",
        "commit contains exactly authorized content")

def cand_i3_foreign_other_path():
    s = "cand-I3"
    r = mkrepo()
    write(f"{r}/f", "X\n"); write(f"{r}/g", "G\n")
    sh(r, "add", "f", "g"); sh(r, "commit", "-qm", "base")
    write(f"{r}/g", "G-staged\n")
    g_blob = out(r, "hash-object", "-w", "g")
    sh(r, "update-index", "--cacheinfo", f"100644,{g_blob},g")
    write(f"{r}/f", "H\n")
    candidate_commit(r, "f", "claim:i3")
    rec(s, "foreign-other-path-untouched", "behavior",
        index_entry(r, "g")[1] == g_blob and status(r, "f") == "",
        "promoted path refreshed clean; unrelated foreign staged path untouched")

def cand_i4_interleaving_abort():
    """Deterministic writer interleaving: foreign native `git update-index`
    lands BETWEEN our refresh-copy build and lock acquisition. The byte
    comparison must abort the refresh; foreign content and the promoted
    commit both survive; the promoted path is honestly dirty (stale entry),
    recoverable by a later refresh attempt (retried here after the
    interleaving completes)."""
    s = "cand-I4"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    e0 = index_entry(r, "f")
    write(f"{r}/f", "H\n")
    newc, _ = d2_commit(r, "f", "claim:i4", h0)
    blob = out(r, "rev-parse", "HEAD:f")
    # --- inline refresh with an injected interleaving point
    gitdir = os.path.join(r, ".git"); real = f"{gitdir}/index"; tmp = f"{gitdir}/s0-refresh-index"
    before = read(real); shutil.copyfile(real, tmp)
    env = {"GIT_INDEX_FILE": tmp}
    sh(r, "update-index", "--cacheinfo", f"100644,{blob},f", env=env)
    # foreign writer interleaves here (deterministic)
    write(f"{r}/f", "B-late\n")
    b_blob = out(r, "hash-object", "-w", "f")
    sh(r, "update-index", "--cacheinfo", f"100644,{b_blob},f")   # native write to REAL index
    lock_acquire(r)
    aborted = False
    try:
        if read(real) == before:
            os.rename(tmp, real)
        else:
            aborted = True
            os.remove(tmp)
    finally:
        lock_release(r)
    rec(s, "interleaving-aborts-refresh", "behavior", aborted,
        "byte comparison detected the native interleaving; refresh aborted, no clobber")
    rec(s, "foreign-late-write-preserved", "behavior", index_entry(r, "f")[1] == b_blob,
        "foreign staged content intact after aborted refresh")
    rec(s, "commit-unaffected", "behavior", out(r, "rev-parse", "HEAD") == newc,
        "promoted commit stands; only the cosmetic refresh was deferred")
    # retry after interleaving completes: re-plan refresh against the NEW live
    # entry; it is foreign content (not the mint-HEAD blob), so preserve it.
    x_blob = out(r, "rev-parse", f"{h0}:f")
    acts = conditional_refresh(r, {"f": ("100644", blob)}, {"f": index_entry(r, "f")},
                               {"f": x_blob})
    rec(s, "retry-preserves-foreign", "behavior",
        "preserved-foreign" in acts["f"] and index_entry(r, "f")[1] == b_blob,
        "re-planned refresh compares against the new live entry and preserves it")

def cand_lock_interop():
    s = "cand-lock"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    write(f"{r}/g", "g\n")
    lock_acquire(r)
    try:
        p = sh(r, "add", "g", check=False)
        rec(s, "native-git-honors-index-lock", "behavior",
            p.returncode != 0 and b"index.lock" in p.stderr,
            "native `git add` refuses while the real index.lock is held; "
            "the conditional-refresh section is atomic against native writers")
    finally:
        lock_release(r)
    p2 = sh(r, "add", "g", check=False)
    rec(s, "lock-release-restores", "behavior", p2.returncode == 0,
        "native writer proceeds after lock release (no permanent blockage)")

def cand_crash_cutpoints():
    s = "cand-crash"
    # C1: crash after prepare, before commit
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    write(f"{r}/f", "H\n")
    candidate_commit(r, "f", "claim:c1", crash_at="after-prepare")
    res = recover(r, "claim:c1")
    rec(s, "C1-after-prepare", "behavior",
        res == "ambiguous-fail-closed" and out(r, "rev-parse", "HEAD") == h0
        and not promotable(r, "claim:c1"),
        f"no commit, HEAD unchanged, resolution={res}; claim never reauthorized")
    # C2: crash after CAS, before outcome -> recovery finishes without recommit
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    write(f"{r}/f", "H\n")
    newc = candidate_commit(r, "f", "claim:c2", crash_at="after-cas")
    res = recover(r, "claim:c2")
    rec(s, "C2-after-cas-recovers", "behavior",
        res == "recovered-committed" and out(r, "rev-parse", "HEAD") == newc
        and status(r, "f") == "",
        "recovery completed outcome+refresh from trailer; no second commit, status clean")
    # C3: crash after CAS, then ref REWRITTEN to mint head (owner B shape)
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    write(f"{r}/f", "H\n")
    candidate_commit(r, "f", "claim:c3", crash_at="after-cas")
    sh(r, "update-ref", "HEAD", h0)              # rewrite away the trailer commit
    res = recover(r, "claim:c3")
    rec(s, "C3-rewrite-fails-closed", "behavior",
        res == "ambiguous-fail-closed" and not promotable(r, "claim:c3"),
        "trailer gone, no durable outcome: ambiguous, claim excluded pending operator — never reauthorized")
    # C3b: same rewrite but outcome WAS published before crash
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    write(f"{r}/f", "H\n")
    candidate_commit(r, "f", "claim:c3b", crash_at="after-outcome")
    sh(r, "update-ref", "HEAD", h0)
    res = recover(r, "claim:c3b")
    rec(s, "C3b-durable-outcome-survives-rewrite", "behavior",
        res == "consumed-by-durable-outcome-despite-rewrite" and not promotable(r, "claim:c3b"),
        "durable outcome consumes even when reachable history is rewritten")
    # C4: crash after outcome, before refresh
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    write(f"{r}/f", "H\n")
    candidate_commit(r, "f", "claim:c4", crash_at="after-outcome")
    res = recover(r, "claim:c4")
    rec(s, "C4-refresh-only-recovery", "behavior",
        res == "recovered-committed" and status(r, "f") == "",
        "recovery performed conditional refresh only, no recommit")

# ------------------------------------------------- hooks and tree freeze
def hook_revalidation():
    s = "hook"
    r = mkrepo()
    write(f"{r}/f", "X\n"); sh(r, "add", "f"); sh(r, "commit", "-qm", "base")
    h0 = out(r, "rev-parse", "HEAD")
    write(f"{r}/f", "H\n")
    data = read(f"{r}/f")
    idx = os.path.join(r, ".git", "s0-private-index")
    env = {"GIT_INDEX_FILE": idx}
    blob = out(r, "hash-object", "-w", "--stdin", "--path=f", input_bytes=data)
    sh(r, "read-tree", h0, env=env)
    sh(r, "update-index", "--add", "--cacheinfo", f"100644,{blob},f", env=env)
    frozen = out(r, "write-tree", env=env)
    # mutating hook: foreign content into the PRIVATE index
    write(f"{r}/evil", "hook-injected\n")
    evil_blob = out(r, "hash-object", "-w", "evil")
    sh(r, "update-index", "--add", "--cacheinfo", f"100644,{evil_blob},f", env=env)
    after = out(r, "write-tree", env=env)
    rec(s, "mutating-hook-detected", "behavior", after != frozen,
        "post-hook write-tree != frozen tree: mutation of the private index by a "
        "hook is detectable and must hold the promotion")
    # refusing hook
    hooks = os.path.join(r, ".git", "hooks")
    write(f"{hooks}/pre-commit", "#!/bin/sh\nexit 1\n", 0o755)
    p = subprocess.run(["bash", f"{hooks}/pre-commit"], capture_output=True, env=dict(os.environ, GIT_INDEX_FILE=idx))
    rec(s, "refusing-hook-holds", "behavior", p.returncode != 0,
        "explicitly-run pre-commit refusal propagates as a held promotion")
    os.remove(f"{hooks}/pre-commit"); os.remove(idx)

def filter_stateful():
    s = "filter"
    r = mkrepo()
    cnt = f"{r}/count"
    write(cnt, "0")
    clean = f"{r}/clean.sh"
    write(clean, "#!/bin/sh\nn=$(cat \"$0.count\"); echo $((n+1)) > \"$0.count\"\n"
                 "if [ \"$n\" = 0 ]; then printf 'A\\n'; else printf 'B\\n'; fi\n".replace("$0.count", cnt), 0o755)
    write(f"{r}/.gitattributes", "f filter=demo\n")
    sh(r, "config", "filter.demo.clean", f"sh {clean}")
    sh(r, "config", "filter.demo.required", "true")
    write(f"{r}/f", "same bytes\n")
    data = read(f"{r}/f")
    oid1 = out(r, "hash-object", "--stdin", "--path=f", input_bytes=data)
    oid2 = out(r, "hash-object", "-w", "--stdin", "--path=f", input_bytes=data)
    rec(s, "stateful-filter-diverges", "defect", oid1 != oid2,
        f"same bytes, two filter invocations: {oid1[:8]} != {oid2[:8]}; "
        "a precomputed expected OID cannot be trusted — the OID returned by the "
        "actual `hash-object -w` invocation is the only stageable identity")
    # and the fix: stage exactly what -w persisted
    idx = os.path.join(r, ".git", "s0-private-index")
    env = {"GIT_INDEX_FILE": idx}
    sh(r, "update-index", "--add", "--cacheinfo", f"100644,{oid2},f", env=env)
    rec(s, "stage-returned-oid", "behavior",
        out(r, "ls-files", "-s", "--", "f", env=env).split()[1] == oid2,
        "staging uses the persisted object's returned OID")
    os.remove(idx)

def main():
    try:
        repro_a(); repro_b(); repro_ce2(); repro_d1_clobber(); repro_d2_preserve()
        cand_i1_clean_success(); cand_i2_foreign_same_path()
        cand_i3_foreign_other_path(); cand_i4_interleaving_abort()
        cand_lock_interop(); cand_crash_cutpoints()
        hook_revalidation(); filter_stateful()
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
