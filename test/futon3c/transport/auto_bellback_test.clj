(ns futon3c.transport.auto-bellback-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.transport.http :as http]))

(def ^:dynamic *ledger-file* nil)

(defn- register-agent!
  [agent-id type]
  (reg/register-agent!
   {:agent-id {:id/value agent-id :id/type :continuity}
    :type type
    :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil})
    :capabilities [:invoke]}))

(defn- create-job!
  [{:keys [job-id agent-id caller prompt surface]
    :or {prompt "do work" surface "bell"}}]
  (#'http/create-invoke-job! {:requested-job-id job-id
                              :agent-id agent-id
                              :prompt prompt
                              :caller caller
                              :surface surface}))

(defn- set-job-field!
  [job-id k v]
  (#'http/update-invoke-jobs-ledger!
   (fn [ledger]
     (assoc-in ledger [:jobs job-id k] v))))

(defn- finalize!
  ([job-id] (finalize! job-id "done" {:ok true :result "short result"}))
  ([job-id terminal-state result]
   (#'http/finalize-invoke-job! job-id terminal-state nil nil result "session-1")))

(defn- job
  [job-id]
  (#'http/get-invoke-job job-id))

(use-fixtures
  :each
  (fn [f]
    (let [tmp (java.io.File/createTempFile "auto-bellback" ".edn")]
      (.delete tmp)
      (binding [*ledger-file* (.getAbsolutePath tmp)]
        (with-redefs-fn {#'http/invoke-jobs-store-path (fn [] *ledger-file*)}
          (fn []
            (reg/reset-registry!)
            (http/reset-invoke-jobs!)
            (try
              (f)
              (finally
                (reg/reset-registry!)
                (http/reset-invoke-jobs!)
                (io/delete-file tmp true)))))))))

(deftest codex-job-bells-back-to-registered-caller-once
  (register-agent! "codex-1" :codex)
  (register-agent! "claude-6" :claude)
  (create-job! {:job-id "job-1" :agent-id "codex-1" :caller "claude-6"})
  (let [enqueued (atom [])]
    (with-redefs-fn {#'http/*enqueue-auto-bellback!* #(swap! enqueued conj %)}
      (fn []
        (finalize! "job-1")
        (finalize! "job-1")))
    (is (= 1 (count @enqueued)))
    (is (= "claude-6" (:caller (first @enqueued))))
    (is (= "auto-bellback-job-1" (:bell-job-id (first @enqueued))))
    (is (str/includes? (:prompt (first @enqueued)) "codex-1 finished job `job-1`"))
    (is (str/includes? (:prompt (first @enqueued)) "state: `done`"))
    (is (= {:sent? true
            :bell-job-id "auto-bellback-job-1"}
           (select-keys (:auto-bellback (job "job-1")) [:sent? :bell-job-id])))))

(deftest non-codex-recipient-does-not-bell-back
  (register-agent! "claude-4" :claude)
  (register-agent! "claude-6" :claude)
  (create-job! {:job-id "job-2" :agent-id "claude-4" :caller "claude-6"})
  (let [enqueued (atom [])]
    (with-redefs-fn {#'http/*enqueue-auto-bellback!* #(swap! enqueued conj %)}
      #(finalize! "job-2"))
    (is (empty? @enqueued))
    (is (nil? (:auto-bellback (job "job-2"))))))

(deftest invalid-callers-do-not-bell-back
  (register-agent! "codex-1" :codex)
  (register-agent! "claude-6" :claude)
  (doseq [[idx caller] (map-indexed vector ["http-caller" "joe" nil "" "   " "codex-1"])]
    (testing (pr-str caller)
      (let [job-id (str "invalid-caller-" idx)
            enqueued (atom [])]
        (create-job! {:job-id job-id :agent-id "codex-1" :caller "claude-6"})
        (set-job-field! job-id :caller caller)
        (with-redefs-fn {#'http/*enqueue-auto-bellback!* #(swap! enqueued conj %)}
          #(finalize! job-id))
        (is (empty? @enqueued))))))

(deftest auto-bellback-job-does-not-recurse
  (register-agent! "codex-1" :codex)
  (register-agent! "claude-6" :claude)
  (create-job! {:job-id "job-3"
                :agent-id "codex-1"
                :caller "auto-bellback"
                :surface "auto-bellback"})
  (let [enqueued (atom [])]
    (with-redefs-fn {#'http/*enqueue-auto-bellback!* #(swap! enqueued conj %)}
      #(finalize! "job-3"))
    (is (empty? @enqueued))))

(deftest feature-flag-off-disables-auto-bellback
  (register-agent! "codex-1" :codex)
  (register-agent! "claude-6" :claude)
  (create-job! {:job-id "job-4" :agent-id "codex-1" :caller "claude-6"})
  (let [enqueued (atom [])]
    (with-redefs-fn {#'http/auto-bellback-enabled? (constantly false)
                    #'http/*enqueue-auto-bellback!* #(swap! enqueued conj %)}
      #(finalize! "job-4"))
    (is (empty? @enqueued))
    (is (nil? (:auto-bellback (job "job-4"))))))

(deftest pure-decision-predicate-covers-gates
  (let [base {:job-id "job-5" :agent-id "codex-1" :caller "claude-6" :state "done"}]
    (is (true? (http/should-auto-bellback? base :codex true true)))
    (is (false? (http/should-auto-bellback? (assoc base :state "running") :codex true true)))
    (is (false? (http/should-auto-bellback? base :claude true true)))
    (is (false? (http/should-auto-bellback? (assoc base :caller nil) :codex true true)))
    (is (false? (http/should-auto-bellback? (assoc base :auto-bellback {:sent? true}) :codex true true)))
    (is (false? (http/should-auto-bellback? base :codex true false)))))
