# Supply the Guide's closure identity

Joe requested that required known identities be supplied, rather than discovered
through expensive database searches. F218's closing Guide spent roughly 406
seconds in paginated memory search while trying to obtain a checker-bound ID.

Inspection found that the Guide's old request contained no trace ID. Its terminal
validator checked only that the returned value was a string. The operational
trace digest is constructed later by `certified-handler`, after Guide submission;
there is no pre-existing checker-result digest for that Guide to find.

Implemented in `live_learning_phases.clj`: new close requests carry `:trace-id`
and `:trace-id-kind :closure-input-reference`. The ID is `apm-close-inputs:` plus
the canonical digest of a version tag and frame ID, problem ID, ledger digest
and exact input receipt IDs. It is computed before dispatch identity is hashed,
and is stable over the same frozen inputs. The prompt displays it and instructs
the Guide to copy it, not search memory or use a Git SHA. Existing immutable
role-card blobs are untouched; the prompt clarifies their misleading wording.

Terminal validation recomputes that reference from the request and requires
exact equality with the returned value. The later combined operational trace
and Lean checker digest remain separate, unchanged and authoritative. Supplying
an input reference does not claim that the operational checker already passed.

Tests exercise real request construction, visible prompt identity, exact echo,
wrong/search/Git/missing IDs, missing request authority, ledger drift, and changed
receipt inputs. Existing close-result and audit controls retain their behavior
with controller-supplied IDs. No database or agent calls are needed in these tests.

Deployment is intentionally pending a safe request boundary: old durable close
requests do not contain this new authority. Reloading the stricter validator
while one is in flight would turn it into an invalid terminal. Before activation,
inspect outstanding close requests and use the canonical reviewed request
migration/supersession lifecycle if any exist; never fill or rewrite their frozen
request/dispatch digest in place. No reload, queue mutation or dispatch was made
by this implementation packet. F218's historic trace remains unchanged, and
its separate watchdog/checker progress-contract conflict is not resolved here.

Validation: `clojure -M:test -n futon3c.apm.live-learning-phases-test` passed
69 tests / 366 assertions. clj-kondo reported zero errors/warnings; the workspace
Emacs parentheses check passed on both changed Clojure files. Diff checks passed.
