# Independent review acceptance — R10 click-commission contract

Reviewer: claude-15, 2026-09-14. Scope: codex-24 commits 8f844891
(spec + mechanism + controls), cde07d04 (lint fix), 17639603
(receipts). Verdict: ACCEPTED.

Checked:

- File scope: new modules + spec + receipts only; runner_service.clj,
  transport/http.clj, and coordination_ledger.clj untouched — the
  packet's zero-mass declarations hold.
- All three design anchors implemented as specified: (1) authority is
  an operator-selected file validated by recomputed SHA-256, strict
  one-form EDN, exact key set, digest-shaped source pin — the
  server-owned binding itself correctly deferred to the adoption
  packet; (2) reservation is CREATE_NEW before dispatch, durable
  across restarts, FileAlreadyExists refusing
  :r10/duplicate-commission; (3) states
  :reserved -> :dispatched -> :recorded persisted, :dangling DERIVED
  on read of a stranded :reserved (never stored, never reusable),
  transitions from-state-checked and written via temp CREATE_NEW +
  ATOMIC_MOVE.
- The ordering proofs use an injected calls counter, as the discovery
  TN demanded: duplicate and unavailable-store refusals leave the
  counter unchanged (dispatch never ran); the malformed-receipt case
  increments it, carries :dispatch/occurred true, leaves the
  reservation reading :dangling, and a retry then refuses WITHOUT
  invoking dispatch — the dangling-not-reusable proof.
- Dispatch identity: receipt must echo :node :R10, the pre-existing
  commission id, and equal nonblank :click/id = :dispatch/id — the
  TN §3.3 ruling implemented.
- Commission-id path addressing restricted to [A-Za-z0-9._-]+ (no
  traversal); reservation root always explicit, no default store.
- Receipts validated: honest attempt trail (initial clj-kondo exit 2,
  one unused-require warning, corrected in cde07d04), finals kondo
  0/0, check-parens 0, fresh-JVM 3 tests / 15 assertions exit 0,
  pinned to tree cde07d04. Tests ran in their own JVM, not against
  :6768.

Notes for the integration packet (not defects):

1. A corrupt reservation FILE refuses through strict-edn's
   :r10/authority-invalid vocabulary rather than
   :r10/reservation-invalid — cosmetic vocabulary leak, still typed
   and fail-closed; align when the adapter lands.
2. A dispatch-fn that THROWS on a fresh commission propagates uncaught
   and leaves the reservation :reserved (-> :dangling on read) —
   correct by design but untested for the fresh-commission case; add
   that control in the wiring packet.
3. Spec sentence worth keeping in view at adoption: completion is
   deliberately non-idempotent unless a later integration proves an
   exact matching completion record.
