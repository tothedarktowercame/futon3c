# E-mfuton-lisp-lifting-20261003 — Lisp lifting boundary contract

**Requisition:** in-progress — dispatched 2026-10-03T00:22:59Z to kimi-2 as invoke-1790986976715-30649-68f36ee9

Owner: codex-18. Executor: kimi-2.

Joe authorized Kimi parallelization on 2026-10-03. Target: Array.extract and independently addressable Array.extract.loop.

## Assignment

Joe authorized using Kimi seats to parallelize the Lisp port strategically. Coordinator/reviewer: codex-18. Repository /home/joe/code/mfuton-sbcl, branch codex-18/probe-checkpoints. Read workspace AGENTS.md. Lean is authority, Python reference ce90ec41 is the implementation oracle. Read lisp/causal-port-audit.json and /home/joe/code/lean-wiring/maintenance/causal/lifted_member_publication.json; use model node IDs, not file counts. The target is Array.extract -> independently addressable Array.extract.loop. Source commit a372214 is the bulk baseline; preserve later fixes in the reference.
This first wave freezes interfaces before concurrent production edits. Do not merely grep and report: execute a relevant existing boundary test or producer trace and retain its output. Do not fake the missing source compiler or handwrite a special-case Array.extract implementation. Do not dispatch other agents. No changes to production code, shared build manifests, trackers, or others' work. No git checkout/reset/stash/worktree/restart. Work only in your assigned lisp/parallel/<lane>/ directory; do not commit (coordinator reviews and commits artifacts). Existing dirty Meta files are unrelated and must remain untouched.
Deliver contract.json with: source function/line and revision, causal node IDs, existing Lisp operations, exact typed input/output representations and ownership, missing operations with dependencies, smallest generic implementation packet with proposed exclusive filenames and acceptance commands, and a list of named consumer claims NOT established. Retain command stdout/stderr/exit code. Input fixtures may come from real Python/native producers but must be labeled as such, never credited as native Lisp output. Use /tmp/mfs-venv/bin/python, MFUTON_HOME=/home/joe/code, MFUTON_LEAN_PARSER_LIB=/home/joe/code/mfuton-linux-binding/shim/.lake/build/lib/libMfutonLeanPyShim_MfutonLeanPyShim.so. Check Lisp using futon4/dev/check-parens.el if you write a Lisp probe. If another assignment is active, report conflict rather than switching it silently.
Report completion to codex-18 via Agency bell, with exact artifact paths and outcome. This is a finite one-turn boundary investigation, not an open-ended full compiler port.

Your lane: lifting. Exclusive output directory: /home/joe/code/mfuton-sbcl/lisp/parallel/lifting
Investigate member_materialization and emitted_alias_publication: inspect local-recursive closure conversion and source declaration emission. Capture a real Python producer example with a captured local, plus returned/passed loop behavior if feasible, and identify the exact input representation for hoisting/publication. Distinguish existing Lisp self-tail-call lowering from closure conversion. Produce an implementation contract for the generic lifting boundary.
