# E-futon1b-postcommit-nested-nil

> **Closed 2026-11-08 (already done).** The upstream fix landed in futon1b on 2026-09-29: `drop-nil-values` removes nil-valued keys recursively before a minted act is written (`futon1b_server.clj:297`), and the failure path now invalidates the type's cached listings. Evidence: futon1b d7b8e95 (on master); test `test_act_ids.clj:106` posts nested nils (`{:who nil :inner {:ref nil}}`) and asserts 200, minted, listed, and the same act on retry (test not rerun here). Checked by c3a in the closure pass (claude-6, FIX/WONTFIX sheet).

**VERDICT:** done, closed 2026-11-08, see the closure note (was, 2026-10-09 provisional: OPEN — Bug documented with fix options; only a client-side workaround landed, the upstream fix (drop nils or nil-insensitive compare) is not recorded as done. _(WM status classification by zai-2, medium confidence; not yet confirmed by the author.)_)

Logged 2026-09-27 by claude-17 (M-象-2000 P3-1). Not scheduled.

POST /api/alpha/hyperedge with `:hx/mint-id` and a nested nil in `:hx/props` (for example
`{:grant/interval {:from "…" :until nil}}`) commits the act, then returns HTTP 503
`:postcommit-missing-act`. The post-commit verification compares the request (which still
has the nils) with the stored document (which has dropped them). The caller is told the
write failed, but the act exists (act:6c2f1392-4489-4e45-9d46-79c59b604471). The throw also
comes before `hx/on-put!` and cache invalidation (futon1b_server.clj ~212–255). A retry with
the same idempotency key returns the same act, so the write is not duplicated.

Fix options: drop nils in the transform before the transaction, or compare nil-insensitively.
Either way, a committed write should not be reported as failed.
Client-side workaround in futon3c grant_record.clj: drop nils before writing.

## Closure criteria (provisional, 2026-10-09)

_Drafted from this document's own stated goals during the War Machine status classification (zai-2, high confidence); not yet confirmed by the author._

- [x] futon1b upstream fix lands (nils dropped in the transform, or nil-insensitive compare) so a committed write is never reported as failed, covered by a test (futon1b d7b8e95)
