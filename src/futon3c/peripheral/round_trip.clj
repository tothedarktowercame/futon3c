(ns futon3c.peripheral.round-trip
  "← verification framework for peripheral behavior.

   The ← operator from reverse-morphogenesis (A11):
     run a peripheral → observe the fruit → apply ← backwards
     → do the inferred constraints match the spec?

   verify-constraints: given a peripheral spec and the evidence entries from
   a peripheral session, check that every action respected the spec's constraints.

   run-and-verify: convenience — runs a peripheral through mock actions, collects
   evidence, then calls verify-constraints. The full ← round-trip in one call.

   Violations are specific and actionable: which tool, which arg, which constraint."
  (:require [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]))

;; =============================================================================
;; verify-constraints — the ← operator as code
;; =============================================================================

(defn- check-tool-set
  "Check that every tool invocation was in the spec's tool set.
   Returns violations (possibly empty)."
  [peripheral-spec evidence-entries]
  (->> evidence-entries
       (filter #(= :step (:evidence/claim-type %)))
       (keep (fn [ev]
               (let [tool (get-in ev [:evidence/body :tool])]
                 (when (and (keyword? tool)
                            (not (tools/allowed? tool peripheral-spec)))
                   {:type :tool-not-allowed
                    :tool tool
                    :evidence-id (:evidence/id ev)
                    :allowed (:peripheral/tools peripheral-spec)}))))
       vec))

(defn- check-scope
  "Check that every tool's args respected scope boundaries.
   Returns violations (possibly empty)."
  [peripheral-spec evidence-entries]
  (->> evidence-entries
       (filter #(= :step (:evidence/claim-type %)))
       (keep (fn [ev]
               (let [tool (get-in ev [:evidence/body :tool])
                     args (get-in ev [:evidence/body :args])]
                 (when (and (keyword? tool) (sequential? args)
                            (not (tools/in-scope? tool args peripheral-spec)))
                   {:type :out-of-scope
                    :tool tool
                    :args args
                    :evidence-id (:evidence/id ev)
                    :scope (:peripheral/scope peripheral-spec)}))))
       vec))

(defn- expected-evidence-type
  "What :evidence/type should entries for this peripheral have?"
  [peripheral-spec]
  (if (= :reflect (:peripheral/id peripheral-spec))
    :reflection
    :coordination))

(defn- check-evidence-type
  "Check that all entries have the correct :evidence/type for this peripheral.
   Returns violations (possibly empty)."
  [peripheral-spec evidence-entries]
  (let [expected (expected-evidence-type peripheral-spec)]
    (->> evidence-entries
         (keep (fn [ev]
                 (when (not= expected (:evidence/type ev))
                   {:type :wrong-evidence-type
                    :expected expected
                    :actual (:evidence/type ev)
                    :evidence-id (:evidence/id ev)})))
         vec)))

(defn- check-structure
  "Check structural invariants: has a :goal start, has a :conclusion stop,
   all entries share the same session-id.
   Returns violations (possibly empty)."
  [evidence-entries]
  (let [claims (map :evidence/claim-type evidence-entries)
        sessions (set (keep :evidence/session-id evidence-entries))
        violations (transient [])]
    (when (not= :goal (first claims))
      (conj! violations {:type :missing-goal
                         :message "First evidence entry should have claim-type :goal"}))
    (when (not= :conclusion (last claims))
      (conj! violations {:type :missing-conclusion
                         :message "Last evidence entry should have claim-type :conclusion"}))
    (when (> (count sessions) 1)
      (conj! violations {:type :inconsistent-session
                         :sessions sessions
                         :message "All entries should share the same session-id"}))
    (persistent! violations)))

(defn verify-constraints
  "Apply ← to a peripheral session's evidence.
   Checks: tool set adherence, scope boundaries, evidence type, structural invariants.
   Returns {:ok true} if all constraints match, or {:ok false :violations [...]}.
   Each violation is a map with :type, specific details, and :evidence-id where applicable."
  [peripheral-spec evidence-entries]
  (let [violations (into []
                         (concat (check-tool-set peripheral-spec evidence-entries)
                                 (check-scope peripheral-spec evidence-entries)
                                 (check-evidence-type peripheral-spec evidence-entries)
                                 (check-structure evidence-entries)))]
    (if (empty? violations)
      {:ok true}
      {:ok false :violations violations})))

;; =============================================================================
;; run-and-verify — full ← round-trip
;; =============================================================================

(defn run-and-verify
  "Run a peripheral through mock actions and verify constraints.
   peripheral: satisfies PeripheralRunner protocol
   context: initial context map (must include :session-id)
   actions: seq of {:tool :keyword :args [...]}
   stop-reason: string

   Runs start → step* → stop, collecting evidence at each stage.
   Then calls verify-constraints on the collected evidence.

   Returns {:ok true :fruit <stop-fruit> :evidence [EvidenceEntry ...]}
   or {:ok false :violations [...] :evidence [...]}
   or {:ok false :error SocialError} if a lifecycle step fails."
  [peripheral context actions stop-reason]
  (let [start-result (runner/start peripheral context)]
    (if (shapes/valid? shapes/SocialError start-result)
      {:ok false :error start-result}
      (let [start-ev (:evidence start-result)]
        (loop [state (:state start-result)
               remaining actions
               evidence [start-ev]]
          (if (empty? remaining)
            ;; All actions done — stop
            (let [stop-result (runner/stop peripheral state stop-reason)]
              (if (shapes/valid? shapes/SocialError stop-result)
                {:ok false :error stop-result :evidence evidence}
                (let [all-evidence (conj evidence (:evidence stop-result))
                      spec (:spec (meta peripheral)
                                  ;; fallback: try to get spec from peripheral
                                  (:spec peripheral))
                      verification (when spec
                                     (verify-constraints spec all-evidence))]
                  (merge {:fruit (:fruit stop-result)
                          :context (:context stop-result)
                          :evidence all-evidence}
                         (if spec
                           verification
                           {:ok true})))))
            ;; Process next action
            (let [action (first remaining)
                  step-result (runner/step peripheral state action)]
              (if (shapes/valid? shapes/SocialError step-result)
                {:ok false :error step-result :evidence evidence}
                (recur (:state step-result)
                       (rest remaining)
                       (conj evidence (:evidence step-result)))))))))))
