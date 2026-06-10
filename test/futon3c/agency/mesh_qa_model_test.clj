(ns futon3c.agency.mesh-qa-model-test
  "Logic model for mesh QA invariants over abstract unified edges.

   This ratifies the MQ invariant set before the implementation. MQ-6 is
   intentionally deferred: current edge data has no per-recipient queue-order
   authority beyond timestamps, so ordering belongs to the Car-3 queue work."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def terminal-states #{"succeeded" "failed" "error" "timeout" "cancelled"})

(defn terminal? [edge]
  (contains? terminal-states (str (:terminal-state edge))))

(defn- by-id [edges]
  (into {} (map (juxt :edge-id identity) edges)))

(defn- compatible-surface? [surface delivery]
  (let [s (some-> surface str)
        d (some-> delivery str)]
    (or (= s d)
        (and (= s "http") (= d "http"))
        (and (= s "bell") (= d "job-status")))))

(defn- codex-recipient? [registry edge]
  (= :codex (get-in registry [:types (:to edge)])))

(defn- unaddressable-caller? [registry caller]
  (let [caller* (some-> caller str str/trim)]
    (and (not (str/blank? (or caller* "")))
         (not (#{"http-caller" "joe"} caller*))
         (not (contains? (:registered registry) caller*)))))

(defn model-violations
  [edges registry-sessions]
  (let [edges-by-id (by-id edges)]
    (vec
     (concat
      (for [e edges
            :when (and (terminal? e) (not (:delivered? e)))]
        {:invariant :MQ-1 :ref (:edge-id e)})
      (for [e edges
            :when (and (= "accepted" (str (:terminal-state e)))
                       (nil? (:timeout-deadline-at e)))]
        {:invariant :MQ-2 :ref (:edge-id e) :capture-gap? true})
      (for [e edges
            :let [orig (get edges-by-id (:bellback-of e))]
            :when (and (:bellback-of e) orig (not= (:to e) (:from orig)))]
        {:invariant :MQ-3 :ref (:edge-id e)})
      (for [e edges
            :let [expected (get-in registry-sessions [:sessions (:to e)])]
            :when (and (:session-id e) expected (not= (:session-id e) expected))]
        {:invariant :MQ-4 :ref (:edge-id e)})
      (for [e edges
            :when (and (terminal? e)
                       (:delivery-surface e)
                       (not (compatible-surface? (:surface e) (:delivery-surface e))))]
        {:invariant :MQ-5 :ref (:edge-id e)})
      (for [e edges
            :when (and (= :invoke-job (:source e))
                       (terminal? e)
                       (codex-recipient? registry-sessions e)
                       (unaddressable-caller? registry-sessions (:from e)))]
        {:invariant :MQ-7 :ref (:edge-id e)})))))

(def conforming-trace
  [{:edge-id "j1" :from "claude-6" :to "codex-1" :surface "bell"
    :terminal-state "succeeded" :delivered? true :delivery-surface "job-status"
    :session-id "s-codex"}
   {:edge-id "j2" :from "codex-1" :to "claude-6" :surface "bell"
    :terminal-state "succeeded" :delivered? true :delivery-surface "job-status"
    :session-id "s-claude" :bellback-of "j1"}])

(deftest conforming-trace-has-no-violations
  (is (empty? (model-violations conforming-trace {:sessions {"codex-1" "s-codex"
                                                             "claude-6" "s-claude"}
                                                  :types {"codex-1" :codex}
                                                  :registered #{"claude-6" "codex-1"}}))))

(deftest adversarial-traces-trip-exactly-one-invariant
  (let [cases {:MQ-1 [{:edge-id "j" :from "a" :to "b" :surface "http"
                       :terminal-state "succeeded" :delivered? false
                       :delivery-surface nil :session-id "s-b"}]
               :MQ-2 [{:edge-id "j" :from "a" :to "b" :surface "http"
                       :terminal-state "accepted" :delivered? false}]
               :MQ-3 [{:edge-id "orig" :from "a" :to "b" :surface "bell"
                       :terminal-state "succeeded" :delivered? true
                       :delivery-surface "job-status"}
                      {:edge-id "back" :from "b" :to "c" :surface "bell"
                       :terminal-state "succeeded" :delivered? true
                       :delivery-surface "job-status" :bellback-of "orig"}]
               :MQ-4 [{:edge-id "j" :from "a" :to "b" :surface "http"
                       :terminal-state "succeeded" :delivered? true
                       :delivery-surface "http" :session-id "wrong"}]
               :MQ-5 [{:edge-id "j" :from "a" :to "b" :surface "irc"
                       :terminal-state "succeeded" :delivered? true
                       :delivery-surface "emacs"}]}]
    (doseq [[invariant trace] cases]
      (is (= [invariant]
             (mapv :invariant (model-violations trace {:sessions {"b" "s-b"}
                                      :types {"b" :claude}
                                      :registered #{"a" "b" "c"}})))
          (str invariant " adversarial trace should trip only itself")))))

(deftest mq7-unaddressable-caller-model
  (let [base {:edge-id "j" :source :invoke-job :from "claude-6" :to "codex-1"
              :surface "bell" :terminal-state "succeeded" :delivered? true
              :delivery-surface "job-status"}
        registry {:sessions {}
                  :types {"codex-1" :codex "claude-1" :claude}
                  :registered #{"claude-6" "codex-1" "claude-1"}}]
    (is (empty? (model-violations [base] registry)))
    (is (= [:MQ-7]
           (mapv :invariant
                 (model-violations [(assoc base :from "claude-missing")] registry))))
    (is (empty? (model-violations [(assoc base :from "http-caller")] registry)))
    (is (empty? (model-violations [(assoc base :from "joe")] registry)))
    (is (empty? (model-violations [(assoc base :from "http-caller" :to "claude-1")]
                                  registry)))))

(deftest mq6-is-deferred-capture-gap
  (is (= :deferred-to-queue-work (get {:MQ-6 :deferred-to-queue-work} :MQ-6))))
