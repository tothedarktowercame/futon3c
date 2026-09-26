(ns futon3c.diagramprover.wm-wire-r9-support
  "Shared hermetic driver for the lane-7b wire tests (E-kimi-task-67,
  PROOF-2a-PLAN <2>3): one tick whose judge-fn throws, in temp stores,
  capturing the finding run-opportunity-core! hands the repair store and
  the record the real repair/record-system-failure! writes of it. The opts
  are the runner test's isolated options as wm-wire-r9-candidate-enact-test
  inlines them (futon2's full-loop-runner-test cannot load here: it reads
  futon2-relative fixtures). Not itself a wire test: no `wire` map, not in
  wm-wire-ledger-test's wire-test-nses."
  (:require [clojure.edn :as edn]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.hermetic-repair-fixture :as hermetic]
            [futon2.aif.learning-trial-ledger :as learning-ledger]
            [futon2.aif.repair-obligation :as repair]
            [futon2.aif.trace :as trace]
            [futon3c.diagramprover.wm-wire :as w]))

(defn run-tick
  "One hermetic tick whose judge-fn throws JUDGE-THROWS. Returns
  {:result run-opportunity!'s result
   :record the tick's run record (edn, read back off disk)
   :finding the map run-opportunity-core! handed the repair store
   :stored the record repair/record-system-failure! durably wrote of it}."
  [judge-throws]
  (let [captured (atom [])
        root (w/tmp-dir "wire-repair")]
    (with-redefs-fn {#'trace/default-trace-dir (w/tmp-dir "wire-trace")
                     #'runner/default-run-record-dir (w/tmp-dir "wire-run-records")
                     #'learning-ledger/default-root (w/tmp-dir "wire-learning")}
      #(binding [runner/*wm-status-reporting?* false]
         (let [result
               (runner/run-opportunity!
                (merge (hermetic/runner-repair-options)
                       {:cohort? false :author "zai-5" :reviewer "codex-7" :repair-reviewer "codex-1"
                        :phase-log-fn (fn [_])
                        :roster-fn (fn [_] {:zai-5 {:status "idle" :invoke-ready? true}
                                            :codex-7 {:status "idle" :invoke-ready? true}
                                            :codex-1 {:status "idle" :invoke-ready? true}})
                        :refresh-fn (fn [])
                        :substrate-preflight-fn (fn [_] {:route :test})
                        :code-state-fn (fn [] {:repo "/futon2" :git-sha "head"
                                               :git-dirty? false :repo-heads {}})
                        :mode-flags-fn (fn [] {}) :version-stamp-fn identity
                        :mission-fn (fn [t] {:id t})
                        :r16-park-fn (fn [_ _] {:ok true :id "park-wire" :status :parked})
                        :delivery-qa-fn (fn [_ _] {:morning-brief/addendum-id "qa-wire"})
                        :queue-fn identity
                        :judge-fn (fn [_] (throw judge-throws))
                        :repair-system-record-fn
                        (fn [m] (let [stored (repair/record-system-failure! root m)]
                                  (swap! captured conj {:finding m :stored stored})
                                  stored))
                        :dispatch-fn (fn [& _] (throw (ex-info "Unexpected dispatch" {})))
                        :construct-fn (fn [& _] (throw (ex-info "stop after selection"
                                                                {:outcome :incomplete})))}))]
           (assoc (first @captured)
                  :result result
                  :record (edn/read-string (slurp (:run-record result)))))))))
