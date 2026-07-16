(ns futon3c.transport.auto-bellback-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.parked-on :as parked-on]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.turn-queue :as turn-queue]
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

(defn- parse-body
  [response]
  (json/parse-string (:body response) true))

(defn- json-request
  [m]
  {:body (json/generate-string m)})

(use-fixtures
  :each
  (fn [f]
    (let [tmp (java.io.File/createTempFile "auto-bellback" ".edn")]
      (.delete tmp)
      (binding [*ledger-file* (.getAbsolutePath tmp)]
        (with-redefs-fn {#'http/invoke-jobs-store-path (fn [] *ledger-file*)}
          (fn []
            (reg/reset-registry!)
            (parked-on/clear!)
            (http/reset-invoke-jobs!)
            (try
              (f)
              (finally
                (reg/reset-registry!)
                (parked-on/clear!)
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
    ;; Format-agnostic: bell-router default flipped ON 2026-06-27, so the prompt is
    ;; "RE: your bell — job `job-1` to codex-1 finished (state `done`)" rather than the
    ;; legacy "codex-1 finished job `job-1` (state: `done`)". Both name the job + state.
    (is (str/includes? (:prompt (first @enqueued)) "job `job-1`"))
    (is (str/includes? (:prompt (first @enqueued)) "codex-1"))
    (is (str/includes? (:prompt (first @enqueued)) "`done`"))
    (is (= {:sent? true
            :bell-job-id "auto-bellback-job-1"}
           (select-keys (:auto-bellback (job "job-1")) [:sent? :bell-job-id])))))

(deftest claude-recipient-bells-back-to-registered-caller
  ;; Widened 2026-06-11 (Joe): claude completions were structurally silent —
  ;; only an agent REMEMBERING to bell covered the gap (it stopped happening
  ;; on warm-pouch turns). Completion contract is structural now.
  (register-agent! "claude-4" :claude)
  (register-agent! "claude-6" :claude)
  (create-job! {:job-id "job-2" :agent-id "claude-4" :caller "claude-6"})
  (let [enqueued (atom [])]
    (with-redefs-fn {#'http/*enqueue-auto-bellback!* #(swap! enqueued conj %)}
      #(finalize! "job-2"))
    (is (= 1 (count @enqueued)))
    (is (= "claude-6" (:caller (first @enqueued))))))

(deftest unregistered-or-ineligible-recipient-does-not-bell-back
  (register-agent! "claude-6" :claude)
  (create-job! {:job-id "job-2b" :agent-id "ghost-9" :caller "claude-6"})
  (let [enqueued (atom [])]
    (with-redefs-fn {#'http/*enqueue-auto-bellback!* #(swap! enqueued conj %)}
      #(finalize! "job-2b"))
    (is (empty? @enqueued))
    (is (nil? (:auto-bellback (job "job-2b"))))))

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

(deftest auto-bellback-job-records-delivery
  ;; Covers the LEGACY (flag-off) lane's synchronous delivery recording; the
  ;; drainer-v2 lane's delivery runs on the drainer thread (finalize-fn) and is
  ;; covered by auto-bellback-routes-through-per-agent-drainer-when-v2-on.
  (System/setProperty "FUTON3C_DRAINER_V2" "false")
  (try
    (register-agent! "claude-6" :claude)
    (let [direct-executor (proxy [java.util.concurrent.AbstractExecutorService] []
                            (shutdown [] nil)
                            (shutdownNow [] [])
                            (isShutdown [] false)
                            (isTerminated [] false)
                            (awaitTermination [_ _] true)
                            (execute [r] (.run r)))]
      (with-redefs [http/invoke-executor direct-executor]
        (let [job-id (#'http/enqueue-auto-bellback!
                      {:caller "claude-6"
                       :bell-job-id "auto-bellback-delivery-1"
                       :prompt "bell back from test"})
              delivery (:delivery (job job-id))]
          (is (= "delivered" (:status delivery)))
          (is (= "auto-bellback" (:surface delivery)))
          (is (= "auto-bellback-ready" (:note delivery))))))
    (finally (System/clearProperty "FUTON3C_DRAINER_V2"))))

(deftest auto-bellback-routes-through-per-agent-drainer-when-v2-on
  ;; I-1 — "single identity is sequential execution" (incident 2026-06-26).
  ;; With drainer-v2 ON (production default), an auto-bellback must enqueue on the
  ;; RECIPIENT's per-agent drainer (single-flight, serialized with its other turns)
  ;; instead of racing on the shared invoke-executor pool. Two concurrent dispatches
  ;; for one agent were the defect that bifurcated claude-11.
  (System/setProperty "FUTON3C_DRAINER_V2" "true")
  (try
    (register-agent! "claude-6" :claude)
    (let [accepted (atom [])
          executor-used (atom false)]
      (with-redefs-fn {#'turn-queue/accept-async!
                       (fn [entry] (swap! accepted conj entry) {:status :accepted})
                       #'http/run-invoke-job! (fn [_] {:ok true})
                       #'http/invoke-executor
                       (proxy [java.util.concurrent.AbstractExecutorService] []
                         (shutdown [] nil) (shutdownNow [] []) (isShutdown [] false)
                         (isTerminated [] false) (awaitTermination [_ _] true)
                         (execute [r] (reset! executor-used true) (.run r)))}
        (fn []
          (#'http/enqueue-auto-bellback!
           {:caller "claude-6" :bell-job-id "ab-route-1" :prompt "bell back"})))
      (is (= 1 (count @accepted)) "routed through the per-agent drainer (accept-async!)")
      (is (= "claude-6" (:to (first @accepted))) "enqueued to the recipient agent's drainer")
      (is (= "auto-bellback" (:from (first @accepted))))
      (is (false? @executor-used) "did NOT use the shared invoke-executor lane"))
    (finally (System/clearProperty "FUTON3C_DRAINER_V2"))))

(deftest auto-bellback-uses-legacy-lane-when-v2-off
  ;; Flag-off fallback stays byte-for-byte: the shared invoke-executor lane.
  (System/setProperty "FUTON3C_DRAINER_V2" "false")
  (try
    (register-agent! "claude-6" :claude)
    (let [accepted (atom [])
          executor-used (atom false)]
      (with-redefs-fn {#'turn-queue/accept-async!
                       (fn [entry] (swap! accepted conj entry) {:status :accepted})
                       #'http/run-invoke-job! (fn [_] {:ok true})
                       #'http/invoke-executor
                       (proxy [java.util.concurrent.AbstractExecutorService] []
                         (shutdown [] nil) (shutdownNow [] []) (isShutdown [] false)
                         (isTerminated [] false) (awaitTermination [_ _] true)
                         (execute [r] (reset! executor-used true) (.run r)))}
        (fn []
          (#'http/enqueue-auto-bellback!
           {:caller "claude-6" :bell-job-id "ab-legacy-1" :prompt "bell back"})))
      (is (true? @executor-used) "flag-off path uses the shared invoke-executor lane")
      (is (empty? @accepted) "flag-off path does NOT route through accept-async!"))
    (finally (System/clearProperty "FUTON3C_DRAINER_V2"))))

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

(deftest parked-caller-resume-suppresses-server-auto-bellback
  (register-agent! "codex-1" :codex)
  (register-agent! "claude-6" :claude)
  (create-job! {:job-id "job-park-1" :agent-id "codex-1" :caller "claude-6"})
  (let [full-result (str "FULL-RESULT-BEGIN\n"
                         (apply str (repeat 80 "long parked reply line\n"))
                         "FULL-RESULT-END")
        park (parked-on/park! {:agent "claude-6"
                               :session "session-1"
                               :surface "emacs-codex-repl"
                               :awaiting ["job-park-1"]
                               :payload "resume checklist"}
                              {:ledger-lookup (constantly nil)
                               :now-ms 1000})
        enqueued (atom [])]
    (with-redefs-fn {#'http/auto-bellback-enabled? (constantly true)
                    #'http/parked-on-enabled? (constantly true)
                    #'http/*enqueue-auto-bellback!* #(swap! enqueued conj %)}
      #(finalize! "job-park-1" "done" {:ok true :result full-result}))
    (is (empty? @enqueued) "server auto-bellback is skipped")
    (let [resume (http/parked-ready-pop! "claude-6" "session-1")]
      (is (= (:id park) (:park-id resume)) "park resume was pushed instead")
      (is (str/includes? (:prompt resume) "resume checklist"))
      (is (str/includes? (:prompt resume) full-result)
          "the substituted park channel carries the complete result, not its summary"))
    (is (= {:suppressed? true
            :reason :parked-on
            :park-id (:id park)}
           (select-keys (:auto-bellback (job "job-park-1"))
                        [:suppressed? :reason :park-id])))))

(deftest no-park-still-auto-bellbacks
  (register-agent! "codex-1" :codex)
  (register-agent! "claude-6" :claude)
  (create-job! {:job-id "job-no-park-1" :agent-id "codex-1" :caller "claude-6"})
  (let [enqueued (atom [])]
    (with-redefs-fn {#'http/auto-bellback-enabled? (constantly true)
                    #'http/parked-on-enabled? (constantly true)
                    #'http/*enqueue-auto-bellback!* #(swap! enqueued conj %)}
      #(finalize! "job-no-park-1"))
    (is (= 1 (count @enqueued)) "no awaiting park preserves the bellback path")
    (is (= "claude-6" (:caller (first @enqueued))))))

(deftest parked-notify-failure-does-not-suppress-auto-bellback
  (register-agent! "codex-1" :codex)
  (register-agent! "claude-6" :claude)
  (create-job! {:job-id "job-park-fail-1" :agent-id "codex-1" :caller "claude-6"})
  (let [enqueued (atom [])]
    (with-redefs-fn {#'http/auto-bellback-enabled? (constantly true)
                    #'http/parked-on-enabled? (constantly true)
                    #'parked-on/note-completion! (fn [& _] (throw (ex-info "boom" {})))
                    #'http/*enqueue-auto-bellback!* #(swap! enqueued conj %)}
      #(finalize! "job-park-fail-1"))
    (is (= 1 (count @enqueued))
        "suppression depends on a successful released-records result; release failure leaves bellback live")))

(deftest pure-decision-predicate-covers-gates
  (let [base {:job-id "job-5" :agent-id "codex-1" :caller "claude-6" :state "done"}]
    (is (true? (http/should-auto-bellback? base :codex true true)))
    (is (false? (http/should-auto-bellback? (assoc base :state "running") :codex true true)))
    (is (true? (http/should-auto-bellback? base :claude true true))
        "claude recipients widened in 2026-06-11")
    (is (false? (http/should-auto-bellback? base nil true true))
        "unregistered/ineligible recipient type never bells")
    (is (false? (http/should-auto-bellback? (assoc base :caller nil) :codex true true)))
    (is (false? (http/should-auto-bellback? (assoc base :auto-bellback {:sent? true}) :codex true true)))
    (is (false? (http/should-auto-bellback? (assoc base :auto-bellback {:suppressed? true})
                                            :codex true true)))
    (is (false? (http/should-auto-bellback? base :codex true true
                                            [{:id "park-1" :agent "claude-6"}]))
        "a released park for the caller suppresses the duplicate server wake")
    (is (true? (http/should-auto-bellback? base :codex true true
                                           [{:id "park-2" :agent "other-agent"}]))
        "a released park for another agent does not suppress this caller")
    (is (false? (http/should-auto-bellback? base :codex true false)))))

;; --- Bell router (E-crossed-bells): explicit, self-describing bellback replies ---

(defn- direct-executor []
  (proxy [java.util.concurrent.AbstractExecutorService] []
    (shutdown [] nil) (shutdownNow [] []) (isShutdown [] false)
    (isTerminated [] false) (awaitTermination [_ _] true)
    (execute [r] (.run r))))

(deftest bell-router-default-on
  ;; Default flipped ON 2026-06-27 (Joe). Explicit off still disables.
  (System/clearProperty "FUTON3C_BELL_ROUTER")
  (try
    (is (true? (#'http/bell-router-enabled?)) "unset ⇒ ON by default")
    (System/setProperty "FUTON3C_BELL_ROUTER" "false")
    (is (false? (#'http/bell-router-enabled?)) "explicit off still disables")
    (finally (System/clearProperty "FUTON3C_BELL_ROUTER"))))

(deftest bell-router-on-makes-bellback-an-explicit-reply
  (System/setProperty "FUTON3C_BELL_ROUTER" "true")
  (try
    (register-agent! "codex-1" :codex)
    (register-agent! "claude-6" :claude)
    (create-job! {:job-id "job-br1" :agent-id "codex-1" :caller "claude-6"})
    (let [enqueued (atom [])]
      (with-redefs-fn {#'http/*enqueue-auto-bellback!* #(swap! enqueued conj %)}
        #(finalize! "job-br1"))
      (let [req (first @enqueued)]
        (is (= "claude-6" (:caller req)) "still delivered to the original caller")
        (is (= "job-br1" (:reply-to req)) "carries the original bell id")
        (is (str/includes? (:prompt req) "RE: your bell"))
        (is (str/includes? (:prompt req) "job `job-br1` to codex-1"))
        (is (str/includes? (:prompt req) "bell or whistle codex-1 directly"))))
    (finally (System/clearProperty "FUTON3C_BELL_ROUTER"))))

(deftest bell-router-on-records-bellback-of-on-the-reply-job
  (System/setProperty "FUTON3C_BELL_ROUTER" "true")
  (try
    (register-agent! "claude-6" :claude)
    (with-redefs [http/invoke-executor (direct-executor)]
      (let [job-id (#'http/enqueue-auto-bellback!
                    {:caller "claude-6" :bell-job-id "auto-bellback-br2"
                     :prompt "RE: your bell ..." :reply-to "job-br2"})]
        (is (= "job-br2" (:bellback-of (job job-id)))
            "the reply job explicitly records the bell it answers")))
    (finally (System/clearProperty "FUTON3C_BELL_ROUTER"))))

(deftest bell-router-off-omits-bellback-of
  (System/setProperty "FUTON3C_BELL_ROUTER" "false")   ;; explicit off (default is now on)
  (System/setProperty "FUTON3C_DRAINER_V2" "false")    ;; legacy lane so invoke-executor redef applies
  (try
    (register-agent! "claude-6" :claude)
    (with-redefs [http/invoke-executor (direct-executor)]
      (let [job-id (#'http/enqueue-auto-bellback!
                    {:caller "claude-6" :bell-job-id "auto-bellback-br3"
                     :prompt "x" :reply-to "job-br3"})]
        (is (nil? (:bellback-of (job job-id))) "off path records no correlation")))
    (finally
      (System/clearProperty "FUTON3C_BELL_ROUTER")
      (System/clearProperty "FUTON3C_DRAINER_V2"))))

(deftest bell-router-surface-header-shows-thread
  (System/setProperty "FUTON3C_BELL_ROUTER" "true")
  (try
    (let [new-req (#'http/wrap-surface-header "body" "bell" "claude-3" nil {:bell-id "J9"})
          reply (#'http/wrap-surface-header "body" "bell" "claude-3" nil
                                            {:bell-id "R9" :in-reply-to "J1"})]
      (is (str/includes? new-req "Thread: bell `J9` — NEW request"))
      (is (str/includes? new-req "in-reply-to=`J9`") "tells the recipient how to reply in-thread")
      (is (str/includes? reply "REPLY to bell `J1`")))
    (finally (System/clearProperty "FUTON3C_BELL_ROUTER"))))

(deftest bell-router-off-omits-thread-line
  (System/setProperty "FUTON3C_BELL_ROUTER" "false")   ;; explicit off (default is now on)
  (try
    (is (not (str/includes?
              (#'http/wrap-surface-header "body" "bell" "claude-3" nil {:bell-id "J9"})
              "Thread:"))
        "off path adds no thread header (byte-for-byte)")
    (finally (System/clearProperty "FUTON3C_BELL_ROUTER"))))

;; --- Reply-delivery dedup (incident 2026-06-26): don't manually re-bell when the
;;     response auto-routes back to the caller (the double-delivery that bifurcated
;;     claude-11). ---

(deftest reply-auto-routes-contract-forbids-manual-rebell
  (System/setProperty "FUTON3C_BELL_ROUTER" "true")
  (try
    (register-agent! "claude-10" :claude)   ;; recipient — eligible type
    (register-agent! "claude-11" :claude)   ;; caller — registered
    (let [hdr (#'http/wrap-surface-header "do the thing" "bell" "claude-11" "claude-10"
                                          {:bell-id "J42"})]
      (is (str/includes? hdr "Reply delivery:") "explicit auto-route contract is shown")
      (is (str/includes? hdr "delivered back to claude-11 automatically"))
      (is (str/includes? hdr "do NOT also bell"))
      (is (str/includes? hdr "Just respond to answer in-thread")
          "NEW-request thread line says respond, not manually bell")
      (is (not (str/includes? hdr "with in-reply-to=`J42`"))
          "the manual reply-bell instruction is suppressed when the reply auto-routes"))
    (finally (System/clearProperty "FUTON3C_BELL_ROUTER"))))

(deftest reply-delivery-contract-shows-even-with-bell-router-off
  ;; The dup happened with bell-router OFF, so the auto-route contract must NOT be
  ;; gated behind it. (Default is now ON, so set it explicitly off here.)
  (System/setProperty "FUTON3C_BELL_ROUTER" "false")
  (try
    (register-agent! "claude-10" :claude)
    (register-agent! "claude-11" :claude)
    (let [hdr (#'http/wrap-surface-header "x" "bell" "claude-11" "claude-10" {:bell-id "J50"})]
      (is (str/includes? hdr "Reply delivery:"))
      (is (str/includes? hdr "do NOT also bell")))
    (finally (System/clearProperty "FUTON3C_BELL_ROUTER"))))

(deftest no-auto-route-keeps-manual-reply-instruction
  ;; When the response will NOT auto-route (caller unregistered), keep the manual
  ;; reply-bell instruction so the answer still reaches the caller.
  (System/setProperty "FUTON3C_BELL_ROUTER" "true")
  (try
    (register-agent! "claude-10" :claude)
    (let [hdr (#'http/wrap-surface-header "x" "bell" "ghost-caller" "claude-10"
                                          {:bell-id "J43"})]
      (is (not (str/includes? hdr "Reply delivery:")) "no auto-route ⇒ no auto-route contract")
      (is (str/includes? hdr "bell/whistle ghost-caller with in-reply-to=`J43`")
          "manual reply-bell instruction retained when there is no auto-route"))
    (finally (System/clearProperty "FUTON3C_BELL_ROUTER"))))

;; --- Typed bells (M-typed-bells): type/ref on the wire + ArSE bridge ---

(deftest typed-bells-default-off
  (System/clearProperty "FUTON3C_TYPED_BELLS")
  (is (false? (#'http/typed-bells-enabled?))))

(deftest typed-bells-off-ignores-type-and-ref
  (System/clearProperty "FUTON3C_TYPED_BELLS")
  (with-redefs-fn {#'http/invoke-executor (direct-executor)
                   #'http/run-invoke-job! (fn [_] {:ok true})
                   #'http/arse-ask! (fn [& _] (throw (ex-info "must not write ArSE" {})))}
    (fn []
      (let [response (#'http/handle-bell
                      (json-request {"agent-id" "claude-6"
                                     "prompt" "Is this wired?"
                                     "caller" "codex-1"
                                     "job-id" "typed-off-1"
                                     "type" "query"
                                     "ref" "ask-existing"})
                      {})
            body (parse-body response)
            j (job "typed-off-1")]
        (is (= 202 (:status response)))
        (is (= "typed-off-1" (:job-id body)))
        (is (not (contains? j :bell-type)))
        (is (not (contains? j :ref)))))))

(deftest typed-bells-rejects-unknown-type
  (System/setProperty "FUTON3C_TYPED_BELLS" "true")
  (try
    (let [response (#'http/handle-bell
                    (json-request {"agent-id" "claude-6"
                                   "prompt" "x"
                                   "type" "frobnicate"})
                    {})]
      (is (= 400 (:status response)))
      (is (= "invalid-bell-type" (:err (parse-body response)))))
    (finally (System/clearProperty "FUTON3C_TYPED_BELLS"))))

(deftest typed-bells-answer-requires-ref
  (System/setProperty "FUTON3C_TYPED_BELLS" "true")
  (try
    (let [response (#'http/handle-bell
                    (json-request {"agent-id" "claude-6"
                                   "prompt" "yes"
                                   "type" "answer"})
                    {})]
      (is (= 400 (:status response)))
      (is (= "answer-ref-required" (:err (parse-body response)))))
    (finally (System/clearProperty "FUTON3C_TYPED_BELLS"))))

(deftest typed-query-creates-arse-thread-and-stamps-job-ref
  (System/setProperty "FUTON3C_TYPED_BELLS" "true")
  (try
    (let [asked (atom [])]
      (with-redefs-fn {#'http/invoke-executor (direct-executor)
                       #'http/run-invoke-job! (fn [_] {:ok true})
                       #'http/arse-ask! (fn [_config req]
                                          (swap! asked conj req)
                                          {:ok true :status 201 :thread-id "ask-typed-1"
                                           :evidence-id "arse-q-ask-typed-1"})}
        (fn []
          (let [response (#'http/handle-bell
                          (json-request {"agent-id" "claude-6"
                                         "prompt" "Is S3 complete?"
                                         "caller" "codex-1"
                                         "job-id" "typed-query-1"
                                         "type" "query"})
                          {})
                body (parse-body response)
                j (job "typed-query-1")]
            (is (= 202 (:status response)))
            (is (= :query (:bell-type j)))
            (is (= "ask-typed-1" (:ref j)))
            (is (= "ask-typed-1" (:ref body)))
            (is (= 1 (count @asked)))
            (is (= "codex-1" (:author (first @asked))))))))
    (finally (System/clearProperty "FUTON3C_TYPED_BELLS"))))

(deftest typed-answer-writes-arse-answer-and-records-ref
  (System/setProperty "FUTON3C_TYPED_BELLS" "true")
  (try
    (let [answered (atom [])]
      (with-redefs-fn {#'http/invoke-executor (direct-executor)
                       #'http/run-invoke-job! (fn [_] {:ok true})
                       #'http/arse-answer! (fn [_config req]
                                             (swap! answered conj req)
                                             {:ok true :status 200 :thread-id (:thread-id req)
                                              :evidence-id "arse-a-ask-typed-1"})}
        (fn []
          (let [response (#'http/handle-bell
                          (json-request {"agent-id" "claude-6"
                                         "prompt" "yes"
                                         "caller" "codex-1"
                                         "job-id" "typed-answer-1"
                                         "type" "answer"
                                         "ref" "ask-typed-1"})
                          {})
                j (job "typed-answer-1")]
            (is (= 202 (:status response)))
            (is (= :answer (:bell-type j)))
            (is (= "ask-typed-1" (:ref j)))
            (is (= [{:thread-id "ask-typed-1" :answer "yes" :author "codex-1"}]
                   @answered))))))
    (finally (System/clearProperty "FUTON3C_TYPED_BELLS"))))

(deftest typed-query-replay-with-same-job-id-does-not-write-second-arse-thread
  (System/setProperty "FUTON3C_TYPED_BELLS" "true")
  (try
    (let [n (atom 0)]
      (with-redefs-fn {#'http/invoke-executor (direct-executor)
                       #'http/run-invoke-job! (fn [_] {:ok true})
                       #'http/arse-ask! (fn [& _]
                                          (swap! n inc)
                                          {:ok true :status 201 :thread-id "ask-once"
                                           :evidence-id "arse-q-ask-once"})}
        (fn []
          (#'http/handle-bell
           (json-request {"agent-id" "claude-6" "prompt" "once"
                          "caller" "codex-1" "job-id" "typed-once"
                          "type" "query"})
           {})
          (let [response (#'http/handle-bell
                          (json-request {"agent-id" "claude-6" "prompt" "once"
                                         "caller" "codex-1" "job-id" "typed-once"
                                         "type" "query"})
                          {})
                body (parse-body response)]
            (is (= 1 @n))
            (is (true? (:reused? body)))
            (is (= "ask-once" (:ref body)))))))
    (finally (System/clearProperty "FUTON3C_TYPED_BELLS"))))

(deftest typed-bells-surface-header-shows-type-and-ref
  (System/setProperty "FUTON3C_TYPED_BELLS" "true")
  (try
    (let [header (#'http/wrap-surface-header
                  "body" "bell" "codex-1" nil
                  {:bell-id "J10" :type :query :ref "ask-typed-1"})]
      (is (str/includes? header "Type: query"))
      (is (str/includes? header "help resolve ArSE `ask-typed-1`")))
    (finally (System/clearProperty "FUTON3C_TYPED_BELLS"))))
