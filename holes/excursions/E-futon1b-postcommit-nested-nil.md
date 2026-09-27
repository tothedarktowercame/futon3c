# E-futon1b-postcommit-nested-nil

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
