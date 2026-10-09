# E-futon1b-postcommit-nested-nil

**VERDICT (2026-10-09, provisional):** OPEN — Bug documented with fix options; only a client-side workaround landed, the upstream fix (drop nils or nil-insensitive compare) is not recorded as done. _(WM status classification by zai-2, medium confidence; not yet confirmed by the author.)_

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

- [ ] futon1b upstream fix lands (nils dropped in the transform, or nil-insensitive compare) so a committed write is never reported as failed, covered by a test
