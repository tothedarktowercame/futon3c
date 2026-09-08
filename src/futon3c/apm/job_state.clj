(ns futon3c.apm.job-state
  "Canonical Agency job-state vocabulary consumed by APM pollers.

   These sets must cover every state the producer persists or presents.
   They did not: the Agency's ledger writes \"delivered\" when a job reaches
   an agent's inbox, and this vocabulary never declared it, so an APM poller
   observing that state classified it :unknown -- which the live job driver
   answers with :live-job-state-unclassified, a fault, for a job that is
   proceeding normally. The conformance fence now compares these sets against
   the producer's own rather than only against each other.")

(def active-states #{:queued :activating :running :overrun :delivered})
(def settling-states #{:delivering})
(def terminal-states
  ;; :succeeded is declared because the producer RECOGNISES it as terminal,
  ;; not because it emits it. Measured 2026-09-08: the string appears in the
  ;; invoke-job code exactly once (the terminal predicate itself), is written
  ;; by no ledger transition, and occurs zero times across every campaign on
  ;; disk. It is very likely a phantom of the same kind as :visibility-lag.
  ;; It is kept anyway, and removed only on its own evidence: the skip-guard
  ;; on the invoke path uses this recognition to refuse to re-run a job that
  ;; already finished, so deleting it on a grep would trade a dead name for a
  ;; duplicate execution.
  ;; :deduped is real and recent -- the Agency finalizes a duplicate msg-id
  ;; that way (13 in the live ledger, most recent 2026-09-08T09:39Z).
  #{:done :succeeded :failed :error :cancelled :timeout :deduped})
(def known-states (into #{} (concat active-states settling-states terminal-states)))

(defn classify [state]
  (cond
    (contains? terminal-states state) :terminal
    (contains? settling-states state) :settling
    (contains? active-states state) :active
    ;; :unknown is kept deliberately. It is the cheap error for a state that
    ;; genuinely is foreign -- a vanished job, a peer running newer code --
    ;; and callers act on it (campaign-reconcile filters on it, the driver
    ;; raises :live-job-state-unclassified). What made it dangerous was that
    ;; a state the producer writes and this vocabulary merely FORGOT was
    ;; indistinguishable from a foreign one. The fence closes that; the
    ;; fallback stays.
    :else :unknown))
