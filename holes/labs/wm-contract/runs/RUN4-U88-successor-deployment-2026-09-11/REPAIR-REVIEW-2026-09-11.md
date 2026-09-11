# Identity/lifecycle repair review — changes required

Reviewed futon2 60a80a3a/6b38e22b and futon3c b83d1eb8. Existing series suite independently passes: 10 tests / 60 assertions. Two independent disposable reproductions nevertheless expose remaining consumer errors. No live application, reconciliation, retry, or reset was performed.

## Qualified repair identity is not threaded through T3

The runner writes repairs using `cohort--attempt`, but phase-context and the T3 observation retain local attempt-id. T3 joins repair records using exact attempt-id equality. The retained reproduction records a real temporary repair then invokes the real checker:

```
local-id-result [{:kind :missing-durable-stop-line, :attempt-id "attempt-001", :statuses #{}}]
qualified-id-result []
```

Required fix: explicitly carry local and global identity, join global consumers with the qualified identity, retain cohort-local checkpoint identity, and test the actual repair writer plus tripwire. Check other joins rather than changing only this assertion.

## Existing-file capacity exception can become fresh dispatch

The service chooses capacity exemption using `.isFile` before the controller's locked lifecycle read. The reproduction plants a temporary started marker, removes it during the server preflight port, and returns zero remaining capacity. Controller then observes no started cell and dispatches:

```
{:status 200 :body {"status":"trial-started", ...} :clicks 1}
```

This is an injected filesystem race test, not a claim it occurred live. Required fix: validated durable started/admission identity must constrain the prepared operation to existing-attempt inspection; disappearance/change must refuse before any reservation/click. Keep this decision server-owned and inaccessible to request JSON. A race-safe locked fresh start remains required.

Reproduction files are retained in review-reproductions. Run qualified-tripwire.clj from futon2 with `clojure -M <path>`. Run lifecycle-marker.clj from futon3c with `clojure -Sdeps '{:aliases {:review {:extra-paths ["test" "dev"]}}}' -M:review <path>`. Fixtures use temporary roots and a counted stub click; they do not invoke a real worker.

## Formal scope and reconciliation

The proposed Lean contract at mathlib4 00eb0c045d distinguishes started inspection from fresh admission and proves distinct qualified pair identities. These failures are implementation-correspondence gaps: local-ID lookup erases part of the identity, and file presence is not a validated started state. Futon2 cf1b9034 maps the required negative controls and consumers.

Do not append a successful task terminal or overwrite the failed binding. The proposed historical close needs independent review of the observed `agent-unavailable` classification, the attempted stop-line repair's actual role availability, and the closure exception. A new explicitly typed infrastructure-reconciliation state may be designed without treating missing evidence as task failure or success. Keeping this attempt unresolved until that contract exists does not mean it must remain unresolved permanently.
