(ns futon3c.peripheral.war-machine-pilot-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.war-machine-pilot-backend :as backend]
            [futon3c.peripheral.war-machine-pilot-shapes :as shapes]))

(def ^:private forbidden-engine-paths
  ["/home/joe/code/futon2/scripts/futon2/report/war_machine.clj"
   "/home/joe/code/futon2/src/futon2/aif/action_proposer.clj"
   "/home/joe/code/futon2/src/futon2/aif/free_energy.clj"
   "/home/joe/code/futon2/src/futon2/aif/engine.clj"])

(deftest wm-i1-engine-paths-unreachable
  (testing "the pilot envelope excludes broad write capabilities and refuses WM engine targets"
    (let [spec (common/load-spec :war-machine-pilot)
          tools (:peripheral/tools spec)]
      (is (= :war-machine-pilot (:peripheral/id spec)))
      (is (not (contains? tools :write)))
      (is (not (contains? tools :bash)))
      (is (contains? tools :bash-readonly))
      (is (= #{:anchor-flip :coherence-row-author :pilot-action}
             shapes/substantive-tools))
      (doseq [path forbidden-engine-paths]
        (testing (str "forbidden engine path " path)
          (is (.exists (io/file path))
              (str "expected real path on disk: " path))
          (let [before (slurp path)
                result (backend/pilot-action {:target-file path
                                              :change-set [{:op :replace-first
                                                            :old-string "("
                                                            :new-string "("}]
                                              :consent-gate-event-id "cg-tripwire"
                                              :rationale "tripwire"})]
            (is (false? (:ok result)))
            (is (str/includes? (:error result)
                               "outside pilot-action v0 allowed roots"))
            (is (= before (slurp path)))))))))

(deftest pilot-i1-tripwires-on-substantive-tools
  (testing "missing consent gate id is rejected"
    (let [result (backend/anchor-flip {:anchor-id "wm-ui-anchor:0010"
                                       :new-status :addressed
                                       :anchors-path "/tmp/nowhere-do-not-touch.edn"})]
      (is (false? (:ok result)))
      (is (= :Pilot-I1 (:pilot-invariant result)))
      (is (str/includes? (:error result) "requires :consent-gate-event-id"))))
  (testing "malformed consent gate id is rejected"
    (let [result (backend/anchor-flip {:anchor-id "wm-ui-anchor:0010"
                                       :new-status :addressed
                                       :consent-gate-event-id "made-up"
                                       :anchors-path "/tmp/nowhere-do-not-touch.edn"})]
      (is (false? (:ok result)))
      (is (= :Pilot-I1 (:pilot-invariant result)))
      (is (str/includes? (:error result) "expected prefix 'cg-'"))))
  (testing "addressed anchors cannot be deleted"
    (let [result (backend/anchor-flip {:anchor-id "wm-ui-anchor:0010"
                                       :new-status :deleted
                                       :consent-gate-event-id "cg-delete-tripwire"
                                       :anchors-path "/home/joe/code/futon5a/data/wm-ui-anchors.edn"})]
      (is (false? (:ok result)))
      (is (= :WM-I5 (:pilot-invariant result)))
      (is (str/includes? (:error result)
                         "cannot delete an :addressed anchor")))))
