(ns futon3c.giraffe.reread-worker
  "One-at-a-time Giraffe reading worker. Not scheduled or started here."
  (:require [cheshire.core :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.giraffe.corpus-generation :as generation]
            [futon3c.giraffe.reread-queue :as queue]))

(def default-max-attempts 3)
(def default-lease-ms (* 45 60 1000))
(def terminal-states #{"done" "failed" "error" "cancelled" "timed-out" "rejected"})

(defn reading-prompt [request]
  (str "Read exactly one Giraffe corpus member. Return ONLY one JSON object.\n\n"
       "Target: " (:target_id request) "\n"
       "Source path: " (:source_path request) "\n"
       "Required source sha256: " (:source_sha request) "\n\n"
       "Follow futon2/holes/labs/wm-contract/reconciliation/"
       "Q1-GIRAFFE-FIELD-STALENESS-zai6-2026-10-31.md. "
       "Return fields id, source, status_as_read, purpose, edges, unresolved. "
       "Every evidence quote must be verbatim and occur in its stated six-line window; "
       "source.path and source.sha256 must exactly equal the values above. "
       "Do not edit the corpus or mint Giraffe."))

(defn- json-record [text]
  (let [candidate (or (second (re-find #"(?s)```(?:json)?\s*(\{.*?\})\s*```" (str text)))
                      (str/trim (str text)))]
    (try (json/parse-string candidate true) (catch Exception _ nil))))

(defn- job-text [job]
  (some (fn [key]
          (let [value (get job key)]
            (cond
              (and (string? value) (not (str/blank? value))) value
              (map? value) (some #(let [nested (get value %)]
                                    (when (and (string? nested)
                                               (not (str/blank? nested)))
                                      nested))
                                 [:text :answer :content :message])
              :else nil)))
        [:result :answer :reply :output :message]))

(defn agency-contact
  "Dispatch through the existing Agency bell CLI with an explicit --from,
  poll the returned job, and decode its single JSON record. Intended for the
  later timer; tests inject a contact function and never call Agency."
  [{:keys [reader-seat caller timeout-ms poll-ms]
    :or {caller "giraffe-reread-worker" timeout-ms (* 40 60 1000) poll-ms 2000}}
   _request prompt]
  (let [sent (shell/sh "python3" "/home/joe/code/futon3c/scripts/agency_send.py"
                       "--from" caller "--to" reader-seat "--kind" "bell"
                       "--mode" "work" :in prompt)
        job-id (some->> (str (:out sent) "\n" (:err sent))
                        (re-find #"invoke-[0-9]+-[0-9]+-[0-9a-f]+"))]
    (if-not (and (zero? (:exit sent)) job-id)
      {:ok false :reason :agency-dispatch-failed}
      (loop [remaining timeout-ms]
        (if (neg? remaining)
          {:ok false :reason :agency-job-timeout :job-id job-id}
          (let [response (try
                           (json/parse-string
                            (slurp (str "http://localhost:7070/api/alpha/invoke/jobs/" job-id))
                            true)
                           (catch Exception _ nil))
                job (or (:job response) response)
                state (some-> (:state job) name)]
            (if (contains? terminal-states state)
              (if (not= "done" state)
                {:ok false :reason :agency-job-failed :job-id job-id :state state}
                (if-let [record (some-> (job-text job) json-record)]
                  {:ok true :record record :job-id job-id}
                  {:ok false :reason :reader-record-invalid-json :job-id job-id}))
              (do (Thread/sleep poll-ms)
                  (recur (- remaining poll-ms))))))))))

(defn- record-matches-claim? [record request]
  (and (= (:target_id request) (:id record))
       (= (:source_path request) (get-in record [:source :path]))
       (= (:source_sha request) (get-in record [:source :sha256]))))

(defn run-once!
  "Claim and process at most one request. CONTACT-FN receives REQUEST and PROMPT.
  It returns {:ok true :record map} or {:ok false :reason keyword}."
  [{:keys [store pointer generations-dir contact-fn now-ms lease-ms max-attempts
           new-item-cap]
    :or {lease-ms default-lease-ms max-attempts default-max-attempts}}]
  (when-not (and (integer? new-item-cap) (not (neg? new-item-cap)))
    (throw (ex-info "A nonnegative new-item claim cap is required"
                    {:kind :giraffe-reread/new-item-cap-required})))
  (if-let [request (queue/claim! store now-ms lease-ms new-item-cap)]
    (let [contact (contact-fn request (reading-prompt request))]
      (cond
        (not (:ok contact))
        {:status :failed
         :request (queue/settle! store request
                                 {:status :failed :reason (or (:reason contact)
                                                             :reader-contact-failed)}
                                 now-ms max-attempts)}

        (not (record-matches-claim? (:record contact) request))
        {:status :failed
         :request (queue/settle! store request
                                 {:status :failed :reason :reader-record-pin-mismatch}
                                 now-ms max-attempts)}

        (not (queue/active-claim? store request))
        {:status :superseded :reason :queue-claim-superseded
         :request (first (queue/requests store {:target-id (:target_id request)}))}

        :else
        (let [published (generation/publish-generation!
                         {:pointer pointer :generations-dir generations-dir
                          :claim {:target-id (:target_id request)
                                  :source-path (:source_path request)
                                  :source-sha (:source_sha request)
                                  :old-sha (:old_sha request)}
                          :replacement (:record contact)})]
          (if (= :published (:status published))
            {:status :landed :publication published
             :request (queue/settle! store request
                                     {:status :landed :digest (:digest published)}
                                     now-ms max-attempts)}
            (let [superseded? (= :giraffe-generation/source-changed (:reason published))
                  outcome {:status (if superseded? :superseded :failed)
                           :reason (:reason published)}]
              {:status (:status outcome) :reason (:reason outcome)
               :publication published
               :request (queue/settle! store request outcome now-ms max-attempts)})))))
    {:status :idle}))
