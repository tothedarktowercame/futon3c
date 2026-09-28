(ns futon3c.diagramprover.wm-wire-selection-out-support
  (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.java.shell :as sh]
            [futon2.aif.policy :as policy] [futon2.aif.enactment-habit :as habit]
            [futon2.aif.cascade-prior :as prior] [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner] [futon2.aif.cascade-problems :as cp]
            [futon2.aif.locator-fixtures :as loc] [futon2.report.war-machine :as wm] [futon2.aif.wm.cascade-decision :as wm-cd]
            [futon2.report.cascade-decision-test :as fixture]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-enact-driver :as driver]
            [futon3c.diagramprover.wm-wire-target-support :as target]
            [futon3c.diagramprover.wm-wire-measured-support :as measured]
            [futon3c.diagramprover.wm-wire-r9-support :as r9]))
(def live-records-read
  (mapv #(assoc % :why "Flight records carry candidate but no attempts, enacted-steps, wc-verdict, grain-gate or judge-refusal. Tick candidate and enacted-steps fields have no same-record dispatch, checker or increment product. No live reader end for wires 1, 2 or 4.") target/live-records-read))
(defn census []
  (mapv (fn [p] (let [r (target/pinned p)]
                 {:path (:path p) :enactments (get-in r [:flight :enactments])
                  :fields (frequencies (mapcat #(when (map? %) (filter #{:wc-verdict :delta :attempts :enacted-steps :candidate} (keys %)))
                                              (tree-seq coll? seq r)))})) live-records-read))
(defn setup []
  (doseq [pin [driver/click-001 driver/click-001-enactment driver/click-001-outcome]]
    (assert (= (:sha256 pin) (w/sha256-file (:path pin)))))
  (let [record (w/read-record (:path driver/click-001))
        cid driver/wc-candidate derivation (get-in record [:decision :selection-certificate :candidate-derivations cid])
        action {:id cid :cascade-id cid :target (:target derivation)
                :precedence (mapv #(assoc (get-in derivation [:interpretations %]) :id %) driver/wc-precedence)}
        d (policy/select-action-cascades [{:cascade true :cascade-id cid :action action :controller-score 0 :rank 1}]
                                        {:beta 1})
        enacted (driver/enact-wc)
        e (:enactment enacted)
        key (prior/policy-key {:mission (:target derivation) :shown driver/wc-precedence :semilattice {}})]
    {:decision d :record (assoc-in record [:decision :selection-law] (:selection-law d))
     :enactment e :identity key :enacted enacted}))
(defn checker [record enactment]
  (let [root (w/tmp-dir "selection-wc-") r (io/file root "click.edn") e (io/file root "enactment.edn")]
    (try
      (spit r (pr-str record)) (spit e (pr-str enactment))
      (let [out (sh/sh "bb" driver/checker-path (str r) (str e) "--wc" "--edn")]
        (assert (zero? (:exit out)) (pr-str out)) (edn/read-string (:out out)))
      (finally (measured/cleanup root)))))
(def produced (delay (setup)))
(defn observe [kind mutation]
  (let [{:keys [decision record enactment identity]} @produced
        cid (get-in decision [:selection-law :candidate])
        value (case kind :verdict (checker record enactment) cid)
        changed (case mutation :none value :absent nil
                      :different (if (= kind :verdict) ["different-checker-failure"] :different-candidate))]
    (case kind
      :candidate-increment
      (let [r (habit/increment (assoc enactment :candidate changed) identity [])]
        {:writer cid :reader (second (:record-id r)) :result r})
      :candidate-checker
      (let [v (checker (assoc-in record [:decision :selection-law :candidate] changed) enactment)]
        ;; An empty W_c verdict proves the selected id equals the enacted id.
        ;; Any failure/non-verdict cannot provide that equality witness.
        {:writer cid :reader (when (= [] v) (:candidate enactment)) :verdict v})
      :verdict
      (let [r (habit/increment enactment identity changed)]
        {:writer value :reader (if (= 1 (:delta r)) [] (or (:wc-failures r) (:wc-verdict r))) :result r}))))
(defn enacted-steps [mutation]
  (let [{:keys [decision]} @produced
        value (get-in decision [:selection-law :enacted-steps])
        changed (case mutation :none value :absent nil :different {:different :step})
        chosen (assoc (runner/chosen-summary decision) :enacted-steps changed)
        seen (atom [])
        r ((fr/enact-fn {:dispatch-step! (fn [s] (swap! seen conj (:pattern s)) {:failed {:reason :offline}})})
           {:target "offline" :flight/id "offline"} {:click-id "offline" :chosen chosen})]
    {:writer value :reader nil :attempted @seen :result r}))
(defn refusal [mutation]
  (let [assembled (cp/assemble {:targets [fixture/tick-1-target] :sources (loc/locate-all fixture/tick-1-sources)})
        e (try (wm-cd/cascade-decision assembled
                 (assoc fixture/live-c-opts :live-c {:derived {:refusals [{:kind :source-not-available}]}}))
               (throw (ex-info "expected a real decision refusal" {}))
               (catch clojure.lang.ExceptionInfo e e))
        _ (assert (= "cascade decision refused" (ex-message e)))
        value (:kind (ex-data e))
        changed (case mutation :none value :absent nil :different :different-refusal)
        carrier (ex-info (ex-message e) (assoc (ex-data e) :kind changed))
        r (r9/run-tick carrier)]
    {:writer value :reader (get-in r [:result :checkpoints :selection :sorry :judge-refusal :kind]) :result r}))
