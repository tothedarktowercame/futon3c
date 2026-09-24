(ns futon3c.wm.run4-historical-action
  "Server-configured runner ports for a pinned historical verification.

  The three refusals below are the guard declining to act on the WRONG
  obligation, or on none. They are typed :guardrail-refusal /
  :trigger-ineligible so futon2's repair-class-for
  (full_loop_runner.clj:3470) files them as :environmental-hold. An
  untyped throw lands on that function's :else branch and mints a
  :machine-failure finding -- a stop-line for the machine correctly
  refusing to do something wrong. That is what happened to
  repair-run4-u88-production-successor-20260911-v2--attempt-001 on
  2026-09-11: \"Historical candidate targets another stop-line\" stopped the
  line and has been open ever since (kimi-2's diagnosis, 2026-09-24).
  The two configuration/authority refusals keep the machine-repair
  contract on purpose: a caller supplying an invalid root or sha is a
  defect, not the guard working."
  (:require [clojure.java.io :as io]
            [futon2.aif.repair-obligation :as repair]))

(defn runner-ports
  [{:keys [repair-root verification-root verification-path verification-sha256]
    :as config}]
  (when-not (= #{:repair-root :verification-root :verification-path
                 :verification-sha256}
               (set (keys config)))
    (throw (ex-info "Historical action configuration invalid" {})))
  (when-not (and (string? repair-root) (string? verification-root)
                 (string? verification-path) (string? verification-sha256)
                 (.isDirectory (io/file repair-root))
                 (.isDirectory (io/file verification-root))
                 (re-matches #"[0-9a-f]{64}" verification-sha256))
    (throw (ex-info "Historical action authority invalid" {})))
  (let [evidence {:verification-root verification-root
                  :path verification-path :sha256 verification-sha256}
        read-candidate #(repair/historical-verification-candidate repair-root evidence)]
    {:historical-verification-candidate-fn
     (fn [obligation]
       (let [candidate (read-candidate)]
         (when-not (= (:repair/id obligation) (:repair/id candidate))
           (throw (ex-info "Historical candidate targets another stop-line"
                           {:outcome :guardrail-refusal
                            :failure-kind :guardrail-refusal
                            :failure-detail :historical-candidate-targets-another-stop-line
                            :obligation (:repair/id obligation)
                            :candidate (:repair/id candidate)})))
         candidate))
     :historical-verification-execute-fn
     (fn [{:keys [execution-identity obligation candidate]}]
       (let [fresh (read-candidate)]
         (when-not (and (= candidate fresh)
                        (= (:repair/id obligation) (:repair/id fresh)))
           (throw (ex-info "Historical candidate changed before execution"
                           {:outcome :guardrail-refusal
                            :failure-kind :guardrail-refusal
                            :failure-detail :historical-candidate-changed-before-execution
                            :obligation (:repair/id obligation)})))
         (repair/commit-historical-verification!
          repair-root execution-identity evidence)))}))

(defn validate-applicable!
  "Read the actual first open stop-line and require this pinned historical
  action to target it. This is read-only and must run before admission."
  [config]
  (let [ports (runner-ports config)
        obligation (first (filter #(and (= :open (:repair/status %))
                                        (not= :environmental-hold
                                              (:repair/class %)))
                                  (repair/open-obligations
                                   (:repair-root config))))]
    (when-not obligation
      (throw (ex-info "Historical action has no open stop-line"
                      {:outcome :trigger-ineligible
                       :failure-kind :trigger-ineligible
                       :failure-detail :no-open-stop-line})))
    ((:historical-verification-candidate-fn ports) obligation)
    {:repair-id (:repair/id obligation)}))
