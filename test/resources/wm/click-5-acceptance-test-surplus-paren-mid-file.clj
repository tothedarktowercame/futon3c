(ns futon2.aif.meta-loop-node-injury-acceptance-test
  "Acceptance test pinning injury/d755c886a22d6fc8c04db7fada64089e07157f62
   (run 2026-10-19-f9abc990-b43a-491b-8f2b-d8b14c7fa796, click wm-click-a3cf0c98-d133-499f-8090-eed79cd56a03).

   Classification: :selection-phase-abort-on-unexercised-required-loop-nodes.
   The selection certificate retained no evidence for the four required loop
   nodes, receipt measured all four :refused, the debugger dwell ended in
   :abort, and terminal routing typed the close :invalid-close-outcome.
   After repair, a selecting decision that retains candidate space, policy
   rollout evidence, posterior/temperature and scored class emission must
   measure :complete with 0 refused, must annotate complete via
   require-complete!, and must terminal-route to :action-receipt."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [futon2.aif.wm.loop-node-exercise :as lne]
            [futon2.aif.wm.terminal-receipt :as tr]
            [futon2.aif.full-loop-runner :as flr]))

(def pinned-record-path
  "/home/joe/code/futon2/data/wm-runs/tick-run-record-2026-10-19-f9abc990-b43a-491b-8f2b-d8b14c7fa796.edn")

(def pinned-record-sha256
  "f6634b0452b8c69e7051041b58b1e936a2b176311058e8921c86fd753867dd3f")

(defn- file-sha256 [^java.io.File f]
  (let [d (java.security.MessageDigest/getInstance "SHA-256")]
    (with-open [in (java.io.FileInputStream. f)]
      (.update d in))
    (apply str (map #(format "%02x" %) (.digest d)))))

(defn- slurp-edn [path]
  (clojure.edn/read-string (slurp path)))

(deftest t1-injured-record-pinned
  (testing "the pinned injured record is byte-identical and shows the injury"
    (let [f (io/file pinned-record-path)]
      (is (.exists f) "pinned run record must exist")
      (is (= pinned-record-sha256 (file-sha256 f)) "pinned run record sha256"))
    (let [record (slurp-edn pinned-record-path)]
      (is (= :incomplete
             (get-in record [:wm/loop-node-exercise-v1 :status]))
          "loop-node exercise was incomplete")
      (is (= {:present 0 :bypassed 0 :refused 4}
             (get-in record [:wm/loop-node-exercise-v1 :counts]))
          "all four required nodes refused")
      (is (= :abort (get-in record [:wm/debugger-dwell-v1 :restart-choice]))
          "debugger dwell ended in abort")
      (is (= 65615 (get-in record [:wm/debugger-dwell-v1 :duration-ms]))
          "debugger dwell duration")
      (is (= :invalid-close-outcome
             (get-in record [:terminal-receipt :failure-kind]))
          "terminal receipt typed invalid-close-outcome"))))

(deftest t2-real-receipt-refuses-empty-selecting-decision
  (testing "real receipt on a selecting decision with no retained evidence refuses all nodes"
    (let [decision {:status "selected"
                    :selection-certificate {}
                    :selection-law {}}
          r (lne/receipt decision)]
      (is (= :incomplete (:status r)))
      (is (= 4 (get-in r [:counts :refused])))
      (is (pos? (count (filter (fn [n] (= :refused (:measurement n)))
                               (:nodes r))))))))

(defn- repaired-selection-body []
  {:ok true
   :selection
   {:status "selected"
    :target "M-aif-policy-conditioned-eig"
    :candidates
    [{:id "cand-head"
      :class-emission {:status :computed
                       :consumed-in-g? true
                       :risk 0.5978370007556204
                       :ambiguity 0.0
                       :model {:kind :class-emission
                               :horizon 2
                               :class-preference {2 {:focused 1.0}}}}}
     {:id "cand-rank2"
      :class-emission {:status :computed
                       :consumed-in-g? true
                       :risk 0.5978370007556204
                       :ambiguity 0.0
                       :model {:kind :class-emission
                               :horizon 2
                               :class-preference {2 {:focused 1.0}}}}}]
    :policy-precision-state {:probabilities {"cand-head" 0.75 "cand-rank2" 0.25}
                             :temperature 2.0}
    :policy-comparison {:status :computed
                        :precedence [[:policy-a :policy-b] [:policy-b :policy-c]]
                        :horizons #{2}}}})

(defn- repaired-record [decision]
  {:status "selected"
   :selection-event {:event :cascade-selected
                     :target "M-aif-policy-conditioned-eig"}
   :decision decision})

(deftest t3-repair-gate-selection-retains-required-evidence
  (testing "strategic-selection! with a retained-evidence selection measures all nodes present"
    (let [invoke-called? (atom false)
          opts {:strategic-selection-invoke-fn
                (fn [_request]
                  (reset! invoke-called? true)
                  (repaired-selection-body))
                :strategic-selection-sleep-fn (fn [_])}
          decision (flr/strategic-selection! opts {:mission "M-aif-policy-conditioned-eig"})]
      (is @invoke-called? "the invoke-fn bridge was used")
      (let [r (lne/receipt decision)]
        (is (= :complete (:status r)) "all required nodes exercised")
        (is (= 4 (get-in r [:counts :present])))
        (is (= 0 (get-in r [:counts :refused]))))
      (let [annotated (lne/require-complete! decision)]
        (is (= :complete (get-in annotated [:wm/loop-node-exercise-v1 :status])))
        (is (= 0 (get-in annotated [:wm/loop-node-exercise-v1 :counts :refused]))))))))

(deftest t4-terminal-routing-injured-vs-repaired
  (testing "terminal receipt routes injured record to failure and repaired record to action-receipt"
    (let [injured (slurp-edn pinned-record-path)
          ir (tr/terminal-receipt injured)]
      (is (= :failure (:kind ir)))
      (is (= :invalid-close-outcome (:failure-kind ir))))
    (let [decision (flr/strategic-selection!
                     {:strategic-selection-invoke-fn (fn [_] (repaired-selection-body))
                      :strategic-selection-sleep-fn (fn [_])}
                     {:mission "M-aif-policy-conditioned-eig"})
          r (tr/terminal-receipt (repaired-record decision))]
      (is (= :action-receipt (:kind r)))
      (is (= "M-aif-policy-conditioned-eig" (get-in r [:action-receipt :target]))))))
