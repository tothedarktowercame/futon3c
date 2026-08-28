(ns futon3c.apm.job-port
  "Single Agency lifecycle producer for APM dispatch observations."
  (:require [futon3c.apm.job-state :as job-state]
            [futon3c.apm.live-preflight-runtime :as runtime]))

(def active-states job-state/active-states)
(def settling-states job-state/settling-states)
(def terminal-states job-state/terminal-states)
(def known-states job-state/known-states)
(def classify-state job-state/classify)

(defn jobs-endpoint
  ([agency-base] (jobs-endpoint agency-base nil))
  ([agency-base limit]
   (str agency-base "/api/alpha/invoke/jobs"
        (when limit (str "?limit=" limit)))))

(defn list-jobs
  ([agency-base] (list-jobs runtime/http-json agency-base nil))
  ([agency-base limit] (list-jobs runtime/http-json agency-base limit))
  ([request-fn agency-base limit]
   (request-fn "GET" (jobs-endpoint agency-base limit) nil)))

(defn cancel!
  ([agency-base job-id reason]
   (cancel! runtime/http-json agency-base job-id reason))
  ([request-fn agency-base job-id reason]
   (let [response (request-fn
                   "POST" (str (jobs-endpoint agency-base) "/" job-id "/cancel")
                   {:reason reason})]
     {:ok (and (= 200 (:http/status response)) (:ok response))
      :job-id job-id :response response})))

(defn announce!
  ([agency-base request] (announce! runtime/http-json agency-base request))
  ([request-fn agency-base {:keys [agent-id prompt surface caller mode job-id]}]
   (let [response
         (request-fn "POST" (str agency-base "/api/alpha/invoke/announce")
                     (cond-> {:agent-id agent-id :prompt prompt
                              :surface (or surface "emacs-repl")
                              :caller (or caller "countdown-control")}
                       mode (assoc :mode mode)
                       job-id (assoc :job-id job-id)))]
     {:ok (and (= 202 (:http/status response)) (:ok response)
               (:accepted response) (string? (:job-id response)))
      :job-id (:job-id response) :state (some-> (:state response) keyword)
      :response response})))

(defn activate!
  ([agency-base request] (activate! runtime/http-json agency-base request))
  ([request-fn agency-base {:keys [agent-id prompt surface caller mode job-id
                                   timeout-ms]}]
   (let [response
         (request-fn "POST" (str agency-base "/api/alpha/invoke/activate")
                     (cond-> {:agent-id agent-id :prompt prompt
                              :surface (or surface "emacs-repl")
                              :caller (or caller "countdown-control")
                              :job-id job-id}
                       mode (assoc :mode mode)
                       timeout-ms (assoc :timeout-ms timeout-ms)))]
     {:ok (and (= 202 (:http/status response)) (:ok response)
               (:accepted response))
      :job-id job-id :state (some-> (:state response) keyword)
      :accepted? (true? (:accepted response))
      :reused? (true? (:reused? response)) :response response})))

(defn observe
  ([agency-base job-id] (observe runtime/http-json agency-base job-id))
  ([request-fn agency-base job-id]
   (let [response (request-fn "GET"
                              (str agency-base "/api/alpha/invoke/jobs/" job-id)
                              nil)
         terminal (runtime/job->terminal response)
         state-class (classify-state (:state terminal))]
     (assoc terminal :ok (= 200 (:http/status response))
            :state/class state-class
            :terminal? (= :terminal state-class)))))

(defn await-terminal!
  "Return the only canonical DispatchObservation. CLIENT timeout is evidence of
   budget exhaustion, never evidence that the job succeeded."
  ([agency-base dispatch]
   (await-terminal! runtime/http-json agency-base dispatch {}))
  ([request-fn agency-base {:keys [job-id activation-accepted?]}
    {:keys [max-polls max-settling-polls poll-ms sleep-fn]
     :or {max-polls 60 max-settling-polls 60 poll-ms 500
          sleep-fn #(Thread/sleep (long %))}}]
   (loop [poll 0 settling-poll 0 states []]
     (let [observation (observe request-fn agency-base job-id)
           states (conj states (:state observation))]
       (cond
         (= :unknown (:state/class observation))
         {:ok false :error/code :job-port-state-unclassified
          :finding {:job-id job-id :state (:state observation)
                    :known-states known-states}
          :dispatch-observation
          {:observation/type :dispatch-observation
           :announced-job-id job-id :activated-job-id job-id
           :activation-accepted (true? activation-accepted?)
           :terminal-job-id nil :terminal-state nil
           :state-sequence states :timeout-treated-as-success false}}

         (:terminal? observation)
         {:ok true
          :dispatch-observation
          {:observation/type :dispatch-observation
           :announced-job-id job-id :activated-job-id job-id
           :activation-accepted (true? activation-accepted?)
           :terminal-job-id (:job-id observation)
           :terminal-state (:state observation)
           :state-sequence states :timeout-treated-as-success false
           :terminal observation}}

         (and (= :settling (:state/class observation))
              (>= settling-poll max-settling-polls))
         {:ok false :error/code :job-port-settling-budget-exhausted
          :dispatch-observation
          {:observation/type :dispatch-observation
           :announced-job-id job-id :activated-job-id job-id
           :activation-accepted (true? activation-accepted?)
           :terminal-job-id nil :terminal-state nil
           :state-sequence states :timeout-treated-as-success false}}

         (>= poll max-polls)
         {:ok false :error/code :job-port-budget-exhausted
          :dispatch-observation
          {:observation/type :dispatch-observation
           :announced-job-id job-id :activated-job-id job-id
           :activation-accepted (true? activation-accepted?)
           :terminal-job-id nil :terminal-state nil
           :state-sequence states :timeout-treated-as-success false}}

         :else
         (do (sleep-fn poll-ms)
             (if (= :settling (:state/class observation))
               (recur poll (inc settling-poll) states)
               (recur (inc poll) settling-poll states))))))))
