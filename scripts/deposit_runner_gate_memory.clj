#!/usr/bin/env -S clojure -M
(ns deposit-runner-gate-memory
  "Deposit one cross-authored, reviewed correction memory for the runner gate."
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.peripheral.memory-lifecycle :as lifecycle]
            [futon3c.peripheral.memory-write :as memory-write]
            [futon3c.substrate.client :as substrate])
  (:import [java.security MessageDigest]
           [java.time Instant]))

(def pattern-id "math/corpus-trust-protocol")

(defn- post-hyperedge! [base-url hyperedge]
  (try
    (let [response (http/post (str base-url "/api/alpha/hyperedge")
                              {:headers {"accept" "application/edn"
                                         "content-type" "application/edn"
                                         "x-penholder" "api"}
                               :body (pr-str hyperedge)
                               :timeout 120000
                               :throw false})]
      (if (<= 200 (:status response) 299)
        {:ok true :hyperedge hyperedge}
        {:ok false :status (:status response) :body (:body response)}))
    (catch Throwable error
      ;; XTDB can commit a hyperedge but miss the HTTP response deadline under
      ;; load. Verify by id through the bounded endpoint read before declaring
      ;; failure; never retry blindly and create a second semantic write.
      (let [visible (first
                     (filter #(= (:hx/id hyperedge) (:hx/id %))
                             (substrate/hyperedges-by-end
                              (first (:hx/endpoints hyperedge))
                              {:limit 50 :timeout-ms 120000})))]
        (if visible
          {:ok true :hyperedge visible :verified-after-timeout true}
          (throw error))))))

(defn- assertion-edge [memory-id hx-id agent job-id name]
  {:hx/id hx-id
   :hx/type :memory/assert
   :hx/endpoints [memory-id agent pattern-id job-id]
   :hx/props
   {:roles {:entry memory-id
            :subjects [agent pattern-id]
            :distills []
            :session job-id
            :patterns [pattern-id]}
    :kind :feedback
    :name name
    :hook (str "Agent " agent " omitted per-memory USED/IGNORED verdicts")
    :volatile? false
    :state :current
    :domain :mathematics
    :attachment-status :proposed}})

(defn- ensure-hyperedge! [base-url hyperedge]
  (let [visible (first
                 (filter #(= (:hx/id hyperedge) (:hx/id %))
                         (substrate/hyperedges-by-end
                          (first (:hx/endpoints hyperedge))
                          {:limit 50 :timeout-ms 120000})))]
    (if visible
      {:ok true :hyperedge visible :visible-after-initial-timeout true}
      (post-hyperedge! base-url hyperedge))))

(defn- sha-prefix [value]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (str value) "UTF-8"))]
    (subs (apply str (map #(format "%02x" (bit-and % 0xff)) digest)) 0 24)))

(defn- review-entry [memory-id review-id job-id missing-ids]
  {:evidence/id review-id
   :evidence/subject {:ref/type :memory :ref/id memory-id}
   :evidence/type :memory
   :evidence/claim-type :observation
   :evidence/author "runner-gate-reviewer"
   :evidence/session-id job-id
   :evidence/at (str (Instant/now))
   :evidence/body
   {:review/event :memory-attachment-review
    :review/memory-id memory-id
    :review/pattern-ids [pattern-id]
    :review/verdict :approve
    :review/witness-status :independently-witnessed
    :review/provenance
    {:kind :deterministic-contract-violation
     :reviewer "runner-gate-reviewer"
     :job-id job-id
     :missing-ids missing-ids
     :warrant "offered receipt versus final-report coverage check"}}
   :evidence/tags [:memory :memory/review :runner-gate]})

(defn -main [& _]
  (let [payload (edn/read-string (slurp *in*))
        {:keys [agent job-id missing-ids feedback substrate-url]} payload
        base-url (or substrate-url (System/getenv "FUTON_SUBSTRATE_URL")
                     "http://127.0.0.1:7073")
        evidence-store (f1b/make-futon1b-backend base-url)
        ctx {:agent-id "runner-gate"
             :session-id job-id
             :domain :mathematics
             :evidence-store evidence-store}
        name (str "correct-use-attribution-" (sha-prefix job-id))
        recorded
        (memory-write/record-memory!
         ctx
         {:name name
          :kind :feedback
          :hook (str "Agent " agent " omitted per-memory USED/IGNORED verdicts")
          :body {:capture-kind :runner-contract-correction
                 :agent agent
                 :job-id job-id
                 :missing-ids missing-ids
                 :correction feedback}
          :subjects [{:ref/type :agent :ref/id agent}
                     {:ref/type :pattern :ref/id pattern-id}]})
        memory-id (:id recorded)
        _ (when-not (:ok recorded)
            (throw (ex-info "correction memory evidence deposit failed" recorded)))
        _ (when (:hx-error recorded)
            (let [retried (ensure-hyperedge!
                           base-url
                           (assertion-edge memory-id (:hx-id recorded) agent job-id name))]
              (when-not (:ok retried)
                (throw (ex-info "correction memory hyperedge retry failed"
                                {:recorded recorded :retried retried})))))
        review-id (str "e-review-runner-gate-" (sha-prefix job-id))
        appended (boundary/append! evidence-store
                                   (review-entry memory-id review-id job-id missing-ids))
        _ (when-not (:ok appended)
            (throw (ex-info "correction review evidence failed" appended)))
        reviewed
        (lifecycle/review-attachment!
         (assoc ctx :agent-id "runner-gate-reviewer")
         {:memory-id memory-id
          :review-evidence-id review-id
          :verdict :approve
         :pattern-ids [pattern-id]}
         {:fetch-hyperedges
          (fn [end options]
            (substrate/hyperedges-by-end
             end (merge {:limit 50 :timeout-ms 120000} options)))
          :post-hyperedge (fn [_ctx hyperedge]
                            (post-hyperedge! base-url hyperedge))})]
    (when-not (:ok reviewed)
      (throw (ex-info "correction attachment review failed" reviewed)))
    (prn {:ok true
          :memory-id memory-id
          :review-evidence-id review-id
          :attachment-status (:attachment-status reviewed)})))

(-main)
