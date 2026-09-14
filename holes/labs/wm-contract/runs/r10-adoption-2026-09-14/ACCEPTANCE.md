# Independent review acceptance — R10 commission adoption

Reviewer: claude-15, 2026-09-14. Scope: codex-24 commits f5b4ceee
(authority file + binding + controls), d0335fb3 (receipts).
Verdict: ACCEPTED.

Checked:

- File scope: exactly three new files in f5b4ceee plus receipts in
  d0335fb3; no existing namespace touched; no other commits between
  00be139c and HEAD. Zero-mass declarations hold.
- Commission file bytes: byte-exact match to the packet's verbatim
  content (xxd-confirmed single trailing newline); issuer "joe",
  source-pin = sha256 of SPEC-r10-click-commission-v1.md at 8f844891,
  single-use scope {:node :R10 :caller :wm/handle-wm-click-start
  :dispatch-count 1}.
- Pinned sha independently recomputed: sha256sum over the committed
  file gives 7c5018493c8b10987277231c15c67b9fc9a2102e2c358c47bd688a2a
  303217df — matches the bellback claim AND the binding's code pin.
- Binding namespace (futon3c.wm.r10-commission-binding): exactly the
  three pinned defs (authority-path, authority-sha256,
  reservation-root) plus authorized-commission delegating to
  load-authorized-commission with the pinned pair only — no extra
  logic, no request-supplied arguments, refusals propagate.
- data/r10-reservations does NOT exist (ls refused) and the receipt
  claims :reservation-root-created? false — consistent.
- Controls: pin-integrity (recomputes the digest over the real file's
  bytes), happy path (exact commission id + :authority/sha256 echo),
  drift refusal (tampered temp copy against the server pin refuses
  :r10/invalid-commission / :authority-sha256-mismatch). Mechanism
  behaviour deliberately not re-tested.
- Receipts: kondo 0/0, tests 2/5 exit 0 in the worktree's own JVM
  (not :6768), tree pinned to f5b4ceee. The receipt's check-parens
  command omitted the --eval '(arxana-check-parens-cli)' driver, so
  its exit 0 was ambiguous; reviewer re-ran the canonical form on all
  three files: OK, exit 0. Not a defect in the files — note for
  future receipts: record the full driver invocation.

The single-use commission is now live authority: reservation keyed by
commission id means it authorizes exactly one real R10 click. The
wiring packet's acceptance run spends it; any rehearsal must use a
temp root and a test commission, never the binding's reservation-root.
