(ns futon3c.transport.delivery-protocol-conformance-test
  "Conformance matrix for invoke completion delivery.

  The job-state axis comes from the canonical producer vocabulary. Caller
  routes for the requested fixtures are read from the live registry; the absence of a
  first-class in-JVM route is recorded as an unconstructable matrix row."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.inbox :as agency-inbox]
            [futon3c.agency.parked-on :as parked-on]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.turn-queue :as turn-queue]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.job-state :as job-state]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.transport.http :as http]))

(def ^:dynamic *ledger-file* nil)
(def ^:dynamic *inbox-root* nil)

(defn- register! [agent-id type delivery-mode invoke-fn]
  (reg/register-agent!
   {:agent-id agent-id :type type :delivery-mode delivery-mode
    :invoke-fn invoke-fn :capabilities [:invoke]}))

(defn- create-job! [job-id caller]
  (#'http/create-invoke-job!
   {:requested-job-id job-id :agent-id "delivery-worker"
    :prompt "conformance" :caller caller :surface "bell"}))

(defn- finalize! [job-id state]
  (#'http/finalize-invoke-job!
   job-id (name state) (when-not (= :done state) (name state))
   nil {:ok (= :done state) :result (pr-str {:outcome state})} "session-1"))

(defn- job [job-id]
  (#'http/get-invoke-job job-id))

(defn- public-job [job-id]
  (#'http/invoke-job-public-view (job job-id)))

(defn- caller-route [caller]
  (if-let [agent (reg/get-agent caller)]
    (:agent/delivery-mode agent)
    :unregistered))

(defn- fixture-caller-routes []
  (into {}
        (map (fn [caller] [(caller-route caller) caller]))
        ["push-caller" "inbox-caller" "non-seat-caller"]))

(defn- observable-action? [shape actions job]
  (case shape
    :push (pos? @actions)
    :inbox (some-> job :delivery :destination io/file .isFile)
    :unregistered false))

(defn- settle! [shape original-job-id request result actions]
  (case shape
    :push
    (do
      (with-redefs [turn-queue/drainer-v2-enabled? (constantly true)
                    turn-queue/accept-async!
                    (fn [entry]
                      (swap! actions inc)
                      ((:finalize-fn entry) result)
                      {:status :accepted})]
        (#'http/enqueue-auto-bellback! request))
      (#'http/record-bell-completion-delivery!
       original-job-id (:caller request) result))

    :inbox
    (do
      (#'http/enqueue-auto-bellback! request)
      (#'http/record-bell-completion-delivery!
       original-job-id (:caller request) result))

    :unregistered nil))

(use-fixtures
  :each
  (fn [f]
    (let [ledger (java.io.File/createTempFile "delivery-conformance" ".edn")
          inbox (.toFile
                 (java.nio.file.Files/createTempDirectory
                  "delivery-conformance-inbox"
                  (make-array java.nio.file.attribute.FileAttribute 0)))]
      (.delete ledger)
      (binding [*ledger-file* (.getAbsolutePath ledger)
                *inbox-root* inbox]
        (with-redefs [http/invoke-jobs-store-path (fn [] *ledger-file*)
                      agency-inbox/inbox-root (fn [] *inbox-root*)]
          (reg/reset-registry!)
          (parked-on/clear!)
          (http/reset-invoke-jobs!)
          (try
            (f)
            (finally
              (reg/reset-registry!)
              (parked-on/clear!)
              (http/reset-invoke-jobs!)
              (io/delete-file ledger true)
              (doseq [file (reverse (file-seq inbox))]
                (io/delete-file file true)))))))))

(deftest terminal-outcome-by-derived-caller-route
  (do
    (register! "delivery-worker" :codex :push (fn [& _] {:result "worker"}))
    (register! "push-caller" :claude :push
               (fn [& _] {:result "received"}))
    (register! "inbox-caller" :mock :inbox
               (fn [& _] (throw (ex-info "pull-only caller invoked" {}))))
    (is (= #{:push :inbox :unregistered}
           (set (keys (fixture-caller-routes)))))
    (doseq [[shape caller] (fixture-caller-routes)
            outcome job-state/terminal-states]
      (testing (str (name shape) " × " (name outcome))
        (let [job-id (str "matrix-" (name shape) "-" (name outcome))
              request (atom nil)
              actions (atom 0)]
          (create-job! job-id caller)
          (with-redefs [http/*enqueue-auto-bellback!* #(reset! request %)]
            (finalize! job-id outcome))
          (when (#{:push :inbox} shape)
            (is (= :settling (job-port/classify-state
                              (keyword (:state (public-job job-id)))))
                "terminal execution is not published while delivery is pending")
            (settle! shape job-id @request {:ok (= :done outcome)} actions))
          (let [durable (job job-id)
                public (public-job job-id)
                terminal (runtime/job->terminal {:job public})
                observation (:trace/delivery-observation durable)
                delivered? (= "delivered" (get-in durable [:delivery :status]))
                action? (observable-action? shape actions durable)]
            (is (not= "pending" (get-in durable [:delivery :status])))
            (is (= (name outcome) (:state public)))
            (is (= :terminal (job-port/classify-state (:state terminal))))
            (is (= observation (:trace/delivery-observation public)))
            (is (= observation (:trace/delivery-observation terminal))
                "ledger -> public view -> APM projection preserves delivery evidence")
            (is (or (not delivered?) action?)
                "delivered implies an observable inbox write or registered push")
            (if (= :unregistered shape)
              (do
                (is (= "delivery-failed" (get-in durable [:delivery :status])))
                (is (= "caller-not-a-registered-seat"
                       (get-in durable [:delivery :note]))))
              (is delivered?))))))))

(deftest producer-vocabulary-is-classified-by-the-canonical-apm-consumer
  (is (= job-state/known-states
         (into #{} (concat job-state/active-states
                           job-state/settling-states
                           job-state/terminal-states))))
  (doseq [state job-state/known-states]
    (is (not= :unknown (job-port/classify-state state)) (name state)))
  (is (= job-state/terminal-states job-port/terminal-states))
  (is (= job-state/settling-states job-port/settling-states))
  (is (= job-state/active-states job-port/active-states)))

(deftest in-jvm-caller-shape-is-not-a-constructable-production-route
  ;; The producer persists only a caller string. Both names below therefore
  ;; select the same unregistered/poll branch: there is no durable field or
  ;; registry value from which a suite can derive "in-JVM" as a distinct shape.
  (is (= :unregistered (caller-route "ftriangle-in-jvm")))
  (is (= :unregistered (caller-route "ordinary-non-seat")))
  (let [producer-outcomes (into job-state/terminal-states
                                job-state/settling-states)]
    (is (= (count producer-outcomes)
           (count (for [outcome producer-outcomes]
                  {:caller-shape :in-jvm
                   :outcome outcome
                   :status :unconstructable
                   :finding :caller-provenance-not-persisted}))))))

(deftest delivering-cells-are-derived-from-the-producer-settling-vocabulary
  (register! "delivery-worker" :codex :push (fn [& _] {:result "worker"}))
  (register! "push-caller" :claude :push (fn [& _] {:result "received"}))
  (register! "inbox-caller" :mock :inbox
             (fn [& _] (throw (ex-info "pull-only caller invoked" {}))))
  (is (= #{:delivering} job-state/settling-states))
  (doseq [[shape caller] (fixture-caller-routes)
          state job-state/settling-states]
    (testing (str (name shape) " × " (name state))
      (let [job-id (str "settling-" (name shape))
            record! (var-get #'http/record-invoke-job-delivery-by-job-id!)]
        (create-job! job-id caller)
        (with-redefs [http/*enqueue-auto-bellback!* (constantly nil)
                      http/record-invoke-job-delivery-by-job-id!
                      (fn [& _] nil)]
          (finalize! job-id :done))
        (is (= state (keyword (:state (public-job job-id)))))
        (is (= :settling
               (job-port/classify-state
                (keyword (:state (public-job job-id))))))
        (record! job-id
                 {:surface "poll" :destination (str "/jobs/" job-id)
                  :delivered? false :note "conformance-settled"})
        (is (= :done (keyword (:state (public-job job-id)))))
        (is (= :terminal
               (job-port/classify-state
                (keyword (:state (public-job job-id))))))))))
