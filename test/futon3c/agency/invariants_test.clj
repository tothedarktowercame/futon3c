(ns futon3c.agency.invariants-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.invariants :as invariants]))

(def ^:private queue-gates
  ["FUTON3C_DURABLE_QUEUE"
   "FUTON3C_DRAINER_V2"
   "FUTON3C_REPL_THROUGH_QUEUE"])

(defn- with-queue-gates
  [settings f]
  (let [old-values (into {}
                         (map (fn [k] [k (System/getProperty k)]))
                         queue-gates)]
    (try
      (doseq [k queue-gates]
        (if-let [v (get settings k)]
          (System/setProperty k v)
          (System/clearProperty k)))
      (f)
      (finally
        (doseq [[k v] old-values]
          (if (some? v)
            (System/setProperty k v)
            (System/clearProperty k)))))))

(deftest queue-hardening-status-passes-when-trio-on
  (with-queue-gates
    {"FUTON3C_DURABLE_QUEUE" "true"
     "FUTON3C_DRAINER_V2" "true"
     "FUTON3C_REPL_THROUGH_QUEUE" "true"}
    (fn []
      (let [status (invariants/queue-hardening-status)]
        (is (true? (:ok? status)))
        (is (= {:durable-queue true
                :drainer-v2 true
                :repl-through-queue true}
               (:gates status)))
        (is (= [] (:degraded status)))))))

(deftest queue-hardening-status-names-degraded-gate
  (with-queue-gates
    {"FUTON3C_DURABLE_QUEUE" "true"
     "FUTON3C_DRAINER_V2" "false"
     "FUTON3C_REPL_THROUGH_QUEUE" "true"}
    (fn []
      (testing "one off gate is red and named"
        (let [status (invariants/queue-hardening-status)]
          (is (false? (:ok? status)))
          (is (= false (get-in status [:gates :drainer-v2])))
          (is (= [:drainer-v2] (:degraded status)))
          (is (re-find #"A3 BOOT INTEGRITY WARNING"
                       (invariants/queue-hardening-warning status))))))))
