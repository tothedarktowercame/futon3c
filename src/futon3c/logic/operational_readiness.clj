(ns futon3c.logic.operational-readiness
  "Diagnostic: run every registered family-check-fn against a stub
   evidence-store and report each outcome. Per D-06 of
   M-bounded-in-flight-state — `:operational-when-enabled` should
   come with proof that the invariant is, in fact, operational when
   enabled (or at least was when it last fired).

   Usage (Drawbridge / dev REPL):
     (require '[futon3c.logic.operational-readiness :as op-rd])
     (op-rd/snapshot)        ;; one row per family-id with outcome
     (op-rd/ready? :metabolic-balance/working-tree)

   The diagnostic is *mechanical* — it simply re-fires each
   registered check-fn and reads its `:outcome`. It does not query
   the futon1a evidence store; the running snapshot is the proof.
   A richer history-based diagnostic (last-N-fires, transition
   counts) follows when M-the-futon-stack's feature registry (Q-19)
   captures it more formally.

   Mission: M-bounded-in-flight-state INSTANTIATE D-06."
  (:require [futon3c.logic.probe :as probe]))

(defn- safe-fire
  "Run CHECK-FN with a stub store. Catches any Throwable so a single
   bad family doesn't sink the snapshot."
  [check-fn]
  (try (check-fn nil)
       (catch Throwable t
         {:outcome :error
          :detail {:exception (str (.getName (class t)) ": "
                                   (.getMessage t))}})))

(defn snapshot
  "Return a vec of {:family-id :outcome :detail :tier?} records — one
   per family-check-fn currently registered in
   `futon3c.logic.probe/family-check-fns`. Order is by family-id name.
   Outcome is one of: :ok :violation :inactive :error."
  []
  (->> @probe/family-check-fns
       (sort-by (fn [[k _]] (str k)))
       (mapv (fn [[fid f]]
               (let [r (safe-fire f)]
                 (cond-> {:family-id fid
                          :outcome (:outcome r)
                          :detail (:detail r)}
                   ;; Surface tier for graduated-drive families
                   ;; (currently only metabolic-balance/working-tree).
                   (:max-tier (:detail r))
                   (assoc :tier (:max-tier (:detail r)))))))))

(defn ready?
  "True iff the most recent fresh fire of FAMILY-ID returned :ok.
   Returns nil when the family-id is not registered."
  [family-id]
  (when-let [f (get @probe/family-check-fns family-id)]
    (= :ok (:outcome (safe-fire f)))))

(defn enabled-but-not-firing-cleanly
  "Return the family-ids that are *registered* (i.e., enabled) but
   whose fresh fire is not :ok. The list this returns names the
   invariants that fail D-06's proof — they are operational-when-
   enabled but, at this moment, are reporting violation/inactive/error.
   Should be empty in steady state."
  []
  (->> (snapshot)
       (remove #(= :ok (:outcome %)))
       (mapv :family-id)))
