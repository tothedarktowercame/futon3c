# Independent review acceptance — r10-caller wiring (commissioned click adapter)

Reviewer: claude-15, 2026-09-14. Scope: codex-24 commits df0e47fb
(wiring), a7a75861 (composition correction), 7747d6e7 (receipts), plus
reviewer fix 8331de51. Verdict: ACCEPTED.

Checked:

- File scope: new adapter + test, the pre-declared read-reservation
  vocabulary fix in r10_commission.clj, and the opt-in branch in
  transport/http.clj only. runner_service.clj and
  coordination_ledger.clj untouched; data/r10-reservations still does
  not exist; the production commission is unspent (no reservation
  anywhere for r10-click-commission-2026-09-14-01).
- Composition order verified against both upstream sources, not the
  adapter's claims: run-scheduled-dispatch! (coordination_ledger.clj)
  does NOT catch dispatch-fn exceptions, so the adapter's typed
  refusals propagate unmasked, and it returns {:ok true ...
  :evidence/id ...} — the adapter's mark-recorded! input is real.
  runner_service !status carries :running? — the busy pre-check reads
  an actual key. Order is: commission validate -> evidence-store
  check -> busy pre-check -> ledger commission validate -> durable
  CREATE_NEW reserve -> click -> receipt validate -> :dispatched ->
  evidence append -> :recorded via mark-recorded!.
- Authority: commission comes only from
  r10-commission-binding/authorized-commission; evidence store resolved
  exactly as http.clj's own evidence-store-for-config (config or
  registry peripheral-config), refusing :r10/evidence-store-missing
  BEFORE reserving; no default-atom fallback anywhere in the adapter.
- Dispatch identity: :dispatch/id = :click/id = the (:click-id) that
  click! itself returned; no parallel id minted. A raced
  :already-running click throws :r10/click-rejected with
  :dispatch/occurred false rather than fabricating a receipt.
- All eight packet controls present with injected calls-counter proofs:
  happy path ends :recorded with the evidence entry present; duplicate,
  missing-store, and busy refuse with zero click invocations AND no
  reservation directory created; fresh-commission click-throw and raced
  rejection both leave :dangling with retry refusing duplicate at one
  invocation; a refusing evidence backend yields :r10/recording-failed
  with the reservation durably :dispatched and retry refusing duplicate
  without re-clicking (the non-misreport property); corrupt reservation
  file reads :r10/reservation-invalid (contract acceptance note 1
  discharged; note 2 discharged by the throw control).
- Tests mint their own temp commissions (issuer "test-operator") and
  temp roots; binding vars only touched via with-redefs; the production
  commission and reservation-root appear in no reserving test.
- Receipts: honest trail retained — initial run failed exactly where
  the composition was wrong (adapter returned dispatch-reserved!'s
  envelope where the ledger required the linked receipt;
  :r10/unlinked-dispatch-receipt in tests.out), corrected in a7a75861;
  initial kondo 2 warnings corrected; finals kondo 0/0 on new files,
  http.clj baseline-delta 0/0 (pre-existing info note unchanged), full
  check-parens driver recorded this time, fresh-JVM 5 tests / 28
  assertions exit 0. Zero-mass claims consistent with my own checks.

Review fix applied by reviewer (8331de51, not re-belled): a payload
combining :r10-commissioned with :run4-pin-ref would have run RUN4
admission and then handed record-click! the ledger's result shape while
burning the single-use commission; the seam now refuses it with status
400 :r10-commissioned-with-run4-pin-ref. kondo/check-parens re-run
clean on http.clj.

Notes for the operator-lane acceptance run (not defects):

1. The commissioned path deliberately ignores legacy payload opts
   (author/reviewer/run-id/trigger) — request data supplies nothing but
   the opt-in flag. Send ONLY {:r10-commissioned true}.
2. All adapter refusals surface as HTTP 409 (:status in refuse!),
   including evidence-store-missing; acceptable, but read the
   :error/code, not the status, when diagnosing.
3. The run must go through the serving JVM reloaded from master, with
   its configured durable evidence store — the run spends the single
   commission; a refused-before-reserve outcome (busy, store missing)
   does NOT burn it and may be retried after fixing the condition.
