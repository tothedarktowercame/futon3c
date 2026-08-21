(ns futon3c.apm.countdown-control
  "Operator-stepped controller for the post-baseline f19--f27 countdown."
  (:require [clojure.edn :as edn]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-postconditions :as postconditions]
            [futon3c.apm.campaign-stepper :as stepper]
            [futon3c.apm.countdown-pre-admission :as admission]
            [futon3c.apm.live-preflight-runtime :as live-preflight-runtime]
            [futon3c.apm.problem-projection :as problem-projection])
  (:import [java.nio.file Path]
           [java.time Instant]))

(def manifest-path "holes/labs/M-apm-demonstration/countdown-10-manifest-v1.edn")
(def contract-path "holes/labs/M-apm-demonstration/frame-cycle-contract-v1.edn")
(def state-directory (Path/of "data/apm-campaigns/countdown-f19-f27-r3"
                              (make-array String 0)))
(def ledger-path (.resolve state-directory "ledger.edn"))
(def certificate-directory (.resolve state-directory "certificates"))
(def projection-directory (.resolve state-directory "projection"))
(def problem-buffer-path (.resolve state-directory "problem-buffer.md"))
(def preflight-state-path (.resolve state-directory "live/preflight.edn"))

(defn- inputs []
  {:manifest (edn/read-string (slurp manifest-path))
   :contract (edn/read-string (slurp contract-path))})

(defn registration-body []
  (let [{:keys [manifest contract]} (inputs)
        units (subvec (:units manifest) 1)
        registered
        (mapv (fn [unit]
                (let [check (admission/validate
                             {:countdown-manifest manifest
                              :cycle-contract contract
                              :frame-id (:frame/id unit)})]
                  (when-not (:ok check)
                    (throw (ex-info "Countdown unit failed pre-admission" check)))
                  {:frame-id (:frame/id unit) :problem-id (:problem/id unit)
                   :arm (:arm unit) :registration-hash (:registration/hash check)
                   :harness-hash (get-in manifest [:apparatus :pin/id])}))
              units)]
    {:series :apm :manifest-hash (:manifest/id manifest)
     :phase-order (:phase-order contract)
     :block-plan [{:block-id (:block/id manifest) :ordinal 1 :units registered}]
     :obligation-plan
     (into {} (map (fn [[phase spec]]
                     [phase (select-keys spec [:kind :role :ordinal])])
                   (:phases contract)))
     :claims-required? true}))

(defn bootstrap! []
  (let [loaded (ledger/read-ledger ledger-path)]
    (cond
      (not (:ok loaded)) loaded
      (seq (:events loaded))
      {:ok (= "apm-countdown-r3" (get-in loaded [:projection :campaign/id]))
       :status :already-registered :projection (:projection loaded)}
      :else
      (let [body (registration-body)
            base {:event/seq 0 :event/type :campaign/registered
                  :event/campaign-id "apm-countdown-r3"
                  :event/actor "countdown-control"
                  :event/at (str (Instant/now)) :event/expected-version 0
                  :event/body body}
            event (assoc base :event/id (machine/ledger-digest [base]))
            empty-projection (machine/projection [])]
        (ledger/compare-and-append! ledger-path 0
                                    (:ledger/digest empty-projection) event)))))

(defn- projection-sink [payload]
  (if (get-in payload [:certificate :active/frame])
    (problem-projection/project-latest!
     {:ledger-path ledger-path :projection-directory projection-directory
      :output-path problem-buffer-path :expected-frame-id "f19"
      :expected-problem-id "a00J01"
      :buffer-sink problem-projection/emacs-buffer-sink})
    {:ok true :projected? false :reason :no-active-frame}))

(defn- gate-provider [{:keys [obligation]}]
  (let [{:keys [manifest contract]} (inputs)
        frame-id (get-in obligation [:obligation/action :frame-id])
        check (when frame-id
                (admission/validate {:countdown-manifest manifest
                                     :cycle-contract contract
                                     :frame-id frame-id}))
        evidence (if frame-id (:checks check)
                     {:manifest-content-addressed? true})]
    [{:gate/id :known-failing-regressions
      :gate/status (if (every? true? (vals evidence)) :pass :fail)
      :gate/evidence {:requirements
                      (mapv (fn [[id pass?]]
                              {:requirement/id id :actual pass? :pass? pass?})
                            evidence)}}]))

(defn- options []
  {:ledger-path ledger-path :certificate-directory certificate-directory
   :projection-directory projection-directory :now-fn #(Instant/now)
   :observation-fn (fn [_] {:binding-response {:ok true :bound? false}
                            :jobs-response {:ok true :jobs []}})
   :gate-provider gate-provider :postcondition-fn postconditions/validate
   :project-fn projection-sink
   :handlers {:open-block (fn [action]
                            {:ok true :certificate
                             {:effect :countdown-block-opened
                              :block-id (:block-id action)}})
              :open-frame (fn [action]
                            {:ok true :certificate
                             {:effect :frame-admitted
                              :frame-id (:frame-id action)
                              :problem-id (:problem-id action)
                              :registration-hash (:registration-hash action)}})}
   :actor "countdown-control"})

(defn inspect! [] (stepper/inspect! (options)))

(defn advance! [expected-kind]
  (let [boot (bootstrap!) inspection (inspect!)]
    (if-not (and (:ok boot) (:ok inspection)
                 (= :ready (:stepper/status inspection))
                 (= expected-kind
                    (get-in inspection [:obligation :obligation/action :kind])))
      {:ok false :error/code :countdown-step-precondition-failed
       :expected expected-kind :inspection inspection}
      (let [issued (stepper/issue-permit
                    {:report (:report inspection) :issuer "joe"
                     :issued-at (str (Instant/now))})]
        (if-not (:ok issued)
          issued
          (stepper/step!
           (assoc (options) :permit (:permit issued)
                  :trusted-permit-id (get-in issued [:permit :permit/id])
                  :trusted-issuer "joe")))))))

(defn live-preflight-inputs []
  (let [{:keys [manifest contract]} (inputs)
        unit (second (:units manifest))
        loaded (ledger/read-ledger ledger-path)
        projection (:projection loaded)
        response (live-preflight-runtime/http-json
                  "GET" "http://localhost:7070/api/alpha/agents/f19-proctor")
        agent (:agent response)
        metadata (:metadata agent)]
    {:contract contract
     :inputs
     {:ledger {:version (:campaign/version projection)
               :phase (get-in projection [:active/frame :phase])
               :claim (:active/claim projection)}
      :unit unit :role-card (get-in manifest [:apparatus :artifacts :proctor])
      :seat {:agent-id (:agent-id response) :type (some-> (:type agent) keyword)
             :frame-id (:frame-id metadata) :invoke-ready? (:invoke-ready? agent)}
      :timeouts {:request-timeout-ms 300000
                 :turn-timeout-ms
                 (get-in metadata [:effective-timeouts :turn-timeout-ms])}}
     :state-path preflight-state-path}))

(defn run-live-preflight! []
  (live-preflight-runtime/run-live! (live-preflight-inputs)))
