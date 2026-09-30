"""Registry-owned pytest plugin (futon3c test-registry, pytest runner).

Loaded by the registry's execution-command as `-p futon3c_pytest_registry_plugin`
with this directory prepended to PYTHONPATH. It is the pytest analogue of
futon3c/test_registry/runner.clj: after the tests it writes the run's load
closure to the file named by FUTON3C_REGISTRY_CLOSURE_OUT (beside the log),
as EDN entries {:ns ... :url "file:..."} — the same shape the Clojure runner
writes, so futon3c.test-registry/closure-from-entries reads both.

What goes into the closure (everything under FUTON3C_REGISTRY_REPO_ROOT):

1. Every imported module whose __file__ is a real file under the repo root,
   captured from sys.modules at session end (imports done inside test bodies
   or fixtures are therefore covered, like the Clojure runner's post-test
   snapshot).
2. Every file the run OPENED, captured by a sys.addaudithook on the "open"
   event. Chosen over monkeypatching builtins.open / os.open / io.open
   because the audit event is raised once at C level for all of them
   (plus io.open_code), so a test using os.fdopen(os.open(...)) or
   pathlib.Path.read_bytes is captured too. Writes fire the same event,
   which is harmless: the entry pins the file's bytes as of session end.
   Non-Python inputs — the mfuton tests reading Lean sources such as
   tools/lean4/src/Init/Prelude.lean — arrive here. Paths are recorded as
   OPENED (NOT canonicalized): tools/lean4 may be a symlink out of the
   repo, and the record keeps the repo-relative link path while the sha
   hashes the target bytes through the link — the same convention as the
   Clojure runner's entry->closure.
3. Every shared library ctypes loaded, captured by the audit event
   "ctypes.dlopen" with a path-like argument under the repo root. Chosen to
   live in the CLOSURE rather than the fingerprint: an edited .so is an
   edited run input, exactly like an edited source file, and pinning it by
   name in the fingerprint would not see an in-place rebuild of the .so.

Excluded: this plugin's own file (the instrument, not the specimen — see
runner-namespace handling in futon3c.test-registry), the closure out-file
itself, __pycache__/.pytest_cache and *.pyc (build scratch that regenerates
on every run), and opened files that no longer exist at session end (a test
temp file already deleted; its bytes cannot be pinned).

Inert without the environment variables, so `pytest -p
futon3c_pytest_registry_plugin` outside the registry just runs tests.

Pinned by `reader-version` in futon3c.test-registry like the Clojure runner:
a change here that can alter what runs or how a run is reported requires a
bump; a comment does not.
"""

import hashlib
import os
import sys
import urllib.parse

_REPO_ROOT = os.environ.get("FUTON3C_REGISTRY_REPO_ROOT")
_OUT = os.environ.get("FUTON3C_REGISTRY_CLOSURE_OUT")
_PLUGIN_FILE = os.path.abspath(__file__)

_opened = set()
_dlopened = set()


def _abspath(path):
    # Lexical normalization only, deliberately NOT realpath: keep the link
    # path so the repo-relative record names the symlink, not its target.
    if not os.path.isabs(path):
        path = os.path.join(os.getcwd(), path)
    return os.path.abspath(path)


def _audit(event, args):
    # An exception raised in an audit hook propagates into the audited call;
    # never let bookkeeping break the run being measured.
    try:
        if event == "open":
            p = args[0] if args else None
            if isinstance(p, str):
                # INPUTS ONLY: a file the run opened for READING. Write-only
                # opens are run OUTPUTS (pytest's cache/nodeids, logs, temp
                # scratch): pinning them would make every run stale itself,
                # because the next run rewrites them. builtins.open/io.open
                # report mode as a string; os.open reports (path, None,
                # flags) with O_WRONLY/O_RDWR as flags bits 1/2.
                mode = args[1] if len(args) > 1 else None
                reading = (isinstance(mode, str) and mode.startswith("r")) or \
                          (isinstance(mode, int) and not (mode & 0x3)) or \
                          (mode is None and len(args) > 2 and
                           isinstance(args[2], int) and not (args[2] & 0x3))
                if reading:
                    _opened.add(_abspath(p))
        elif event == "ctypes.dlopen":
            p = args[0] if args else None
            if isinstance(p, str) and ("/" in p or p.endswith(".so")):
                _dlopened.add(_abspath(p))
    except Exception:
        pass


def _under_repo(path):
    root = _abspath(_REPO_ROOT)
    return path == root or path.startswith(root + os.sep)


def _excluded(path):
    if path in (_PLUGIN_FILE, _abspath(_OUT)):
        return True
    if "__pycache__" in path or ".pytest_cache" in path:
        return True
    base = os.path.basename(path)
    return base.endswith((".pyc", ".pyo"))


def _sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(65536), b""):
            h.update(chunk)
    return h.hexdigest()


def _edn_str(s):
    out = s.replace("\\", "\\\\").replace('"', '\\"')
    out = out.replace("\n", "\\n").replace("\t", "\\t").replace("\r", "\\r")
    return '"' + out + '"'


def closure_entries():
    """[{ns url}] for everything under the repo root the run touched."""
    entries = {}
    for name, mod in sorted(sys.modules.items()):
        f = getattr(mod, "__file__", None)
        if not isinstance(f, str):
            continue
        if not f.endswith((".py", ".pyd", ".so")):
            continue
        p = _abspath(f)
        if _under_repo(p) and not _excluded(p) and os.path.isfile(p):
            entries[p] = "module:" + name
    for p in sorted(_opened):
        if _under_repo(p) and not _excluded(p) and os.path.isfile(p):
            entries.setdefault(p, "open:" + p)
    for p in sorted(_dlopened):
        if _under_repo(p) and not _excluded(p) and os.path.isfile(p):
            entries.setdefault(p, "dlopen:" + p)
    lines = ["{:ns %s :url %s}" % (_edn_str(ns),
                                   _edn_str("file://" + urllib.parse.quote(p)))
             for p, ns in sorted(entries.items())]
    return "[" + " ".join(lines) + "]"


def pytest_sessionstart(session):
    sys.addaudithook(_audit)


def pytest_sessionfinish(session, exitstatus):
    if _REPO_ROOT and _OUT:
        with open(_OUT, "w") as fh:
            fh.write(closure_entries())
