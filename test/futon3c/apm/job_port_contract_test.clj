(ns futon3c.apm.job-port-contract-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.registry :as registry]
            [futon3c.apm.job-port :as sut]
            [futon3c.transport.http :as http]))

(defn- handler-request [handler method url payload]
  (let [uri (.getPath (java.net.URI. url))
        response (handler {:request-method (keyword (.toLowerCase method))
                           :uri uri
                           :body (when payload
                                   (io/input-stream
                                    (.getBytes (json/generate-string payload)
                                               "UTF-8")))})]
    (assoc (json/parse-string (:body response) true)
           :http/status (:status response))))

(deftest job-port-contract-real-handler
  (let [runs (atom 0)
        handler (http/make-handler {})
        request-fn (partial handler-request handler)
        authority {:agent-id "job-port-contract-agent"
                   :prompt "contract work" :mode "brief"
                   :model "gpt-5.6-sol"}]
    (registry/register-agent!
     {:agent-id {:id/value (:agent-id authority) :id/type :continuity}
      :type :codex :capabilities [:explore]
      :invoke-fn (fn [_ _]
                   (swap! runs inc)
                   {:result "{:ok true}" :session-id "contract-session"})})
    (let [announced (sut/announce! request-fn "http://handler" authority)
          queued (sut/observe request-fn "http://handler" (:job-id announced))]
      (is (= :queued (:state queued)))
      (is (zero? @runs))
      (let [
          activated (sut/activate! request-fn "http://handler"
                                   (assoc authority :job-id (:job-id announced)))
          terminal (sut/await-terminal!
                    request-fn "http://handler"
                    {:job-id (:job-id announced)
                     :activation-accepted? (:accepted? activated)}
                    ;; The original 100 polls x 1ms gave the async dispatch a
                    ;; 100ms budget, which it does not meet: the handler had
                    ;; simply not run yet when the assertions fired, so runs
                    ;; was 0 and the terminal was absent. This is a real-time
                    ;; budget, not a synchronisation point — kept generous so
                    ;; it fails only when dispatch is genuinely broken.
                    {:max-polls 200 :max-settling-polls 600 :poll-ms 25})]
        (is (:accepted? activated))
        (is (:ok terminal) (pr-str terminal))
        (is (= 1 @runs))
        (is (= (:job-id announced)
               (get-in terminal [:dispatch-observation :terminal-job-id])))
        (is (= "gpt-5.6-sol"
               (get-in terminal [:dispatch-observation :terminal
                                 :invocation/model])))
        (is (false? (get-in terminal
                            [:dispatch-observation :timeout-treated-as-success])))))))

(deftest client-budget-exhaustion-is-never-success
  (let [request-fn (fn [_ _ _]
                     {:http/status 200
                      :job {:job-id "awaiting" :agent-id "agent"
                            :state "running"}})
        result (sut/await-terminal!
                request-fn "http://handler"
                {:job-id "awaiting" :activation-accepted? true}
                {:max-polls 1 :poll-ms 0 :sleep-fn (fn [_])})]
    (is (= :job-port-budget-exhausted (:error/code result)))
    (is (false? (get-in result
                        [:dispatch-observation :timeout-treated-as-success])))))

(deftest delivering-is-settling-and-unknown-states-fail-loudly
  (let [responses (atom [{:http/status 200
                          :job {:job-id "settling" :state "delivering"}}
                         {:http/status 200
                          :job {:job-id "settling" :state "done"
                                :trace/delivery-observation
                                {:terminal-job-id "settling"
                                 :delivery-status "delivered"
                                 :inbox-file-created? true
                                 :registered-push-performed? false
                                 :polling-available? true}}}])
        request-fn (fn [_ _ _]
                     (let [response (first @responses)]
                       (swap! responses #(if (next %) (vec (next %)) %))
                       response))
        result (sut/await-terminal!
                request-fn "http://handler"
                {:job-id "settling" :activation-accepted? true}
                {:max-polls 2 :poll-ms 0 :sleep-fn (fn [_])})]
    (is (:ok result))
    (is (= [:delivering :done]
           (get-in result [:dispatch-observation :state-sequence]))))
  (let [result (sut/await-terminal!
                (fn [_ _ _]
                  {:http/status 200
                   :job {:job-id "novel" :state "new-vocabulary"}})
                "http://handler"
                {:job-id "novel" :activation-accepted? true}
                {:max-polls 100 :poll-ms 0 :sleep-fn (fn [_])})]
    (is (= :job-port-state-unclassified (:error/code result)))
    (is (= [:new-vocabulary]
           (get-in result [:dispatch-observation :state-sequence])))))
