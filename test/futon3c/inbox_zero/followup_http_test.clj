(ns futon3c.inbox-zero.followup-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.followup-queue :as queue]
            [futon3c.agency.parked-on :as parked-on]
            [futon3c.agency.registry :as reg]
            [futon3c.inbox-zero.followup-validity :as validity]
            [futon3c.transport.http :as http])
  (:import [java.io ByteArrayInputStream]))

(defn- request [handler method uri body query]
  (handler (cond-> {:request-method method :uri uri}
             query (assoc :query-string query)
             body (assoc :body (ByteArrayInputStream.
                                (.getBytes (json/generate-string body) "UTF-8"))))))

(defn- body [response] (json/parse-string (:body response) true))

(deftest ready-batch-preserves-the-single-seat-response-bodies
  ;; The handlers share the exact map-producing functions; this route-level
  ;; check pins the response envelope and request order without consuming a
  ;; real lease twice.
  (let [parked (fn [agent session]
                 {:ok true :ready [{:park-id (str agent "-" session)}] :leased true})
        followups (fn [_agent session]
                    {:ok true :ready [{:followup-id (str session "-followup")}] :leased true})]
    (with-redefs-fn {#'http/parked-ready-response parked
                     #'http/followup-ready-response followups}
      (fn []
        (let [handler (http/make-handler {})
              agent "batch-seat"
              session "batch-session"
              expected-parked (body (request handler :get "/api/alpha/parked/ready" nil
                                             "agent=batch-seat&session=batch-session"))
              expected-followups (body (request handler :get "/api/alpha/followups/ready" nil
                                               "agent=batch-seat&session=batch-session"))
              batch (body (request handler :post "/api/alpha/ready/batch"
                                   {:seats [{:agent agent :session session}]} nil))]
          (is (= {:ok true
                  :seats [{:agent agent :session session
                           :parked expected-parked :followups expected-followups}]}
                 batch)))))))

(deftest ready-batch-leases-and-acks-through-the-existing-inboxes
  (let [path (str (System/getProperty "java.io.tmpdir")
                  "/futon3c-ready-batch-test-" (random-uuid) ".edn")]
    (binding [queue/*path-override* path]
      (queue/clear!)
      (parked-on/clear!)
      (reg/reset-registry!)
      (reg/register-agent!
       {:agent-id {:id/value "batch-seat" :id/type :continuity}
        :type :codex :session-id "batch-session" :capabilities [:edit]
        :invoke-fn (fn [_ _] {:result "ok"})})
      (parked-on/ready-push! "batch-seat" "batch-session" "park-batch-1" "resume")
      (queue/enqueue! {:agent "batch-seat" :session "batch-session"
                       :type :inbox-zero :dedupe-key ["batch" "one"] :prompt "follow up"})
      (with-redefs-fn {#'http/parked-on-enabled? (constantly true)}
        (fn []
          (let [handler (http/make-handler {})
                poll #(body (request handler :post "/api/alpha/ready/batch"
                                     {:seats [{:agent "batch-seat" :session "batch-session"}
                                              {:agent "empty-seat" :session "empty-session"}]}
                                     nil))
                first-poll (poll)
                seat (first (:seats first-poll))
                park-id (get-in seat [:parked :ready 0 :park-id])
                followup-id (get-in seat [:followups :ready 0 :followup-id])
                second-poll (poll)]
            (is (= "park-batch-1" park-id))
            (is (string? followup-id))
            (is (empty? (get-in second-poll [:seats 0 :parked :ready]))
                "a batch lease is not delivered again before its lease expires")
            (is (empty? (get-in second-poll [:seats 0 :followups :ready])))
            (is (empty? (get-in second-poll [:seats 1 :parked :ready])))
            (is (empty? (get-in second-poll [:seats 1 :followups :ready])))
            (is (= {:ok true :acked true}
                   (body (request handler :post "/api/alpha/parked/ready/ack"
                                  {:park-id park-id} nil))))
            (is (= {:ok true :acked true}
                   (body (request handler :post "/api/alpha/followups/ready/ack"
                                  {:followup-id followup-id} nil))))
            (is (not (contains? (:leased (queue/snapshot)) followup-id)))
            (reg/report-external-invoke! "batch-seat" "batch-busy"
                                         {:status :invoking :session-id "batch-session"})
            (parked-on/ready-push! "batch-seat" "batch-session" "park-batch-2" "busy")
            (queue/enqueue! {:agent "batch-seat" :session "batch-session"
                             :type :inbox-zero :dedupe-key ["batch" "busy"] :prompt "busy"})
            (let [busy (first (:seats (poll)))
                  parked-get (body (request handler :get "/api/alpha/parked/ready" nil
                                            "agent=batch-seat&session=batch-session"))
                  followup-get (body (request handler :get "/api/alpha/followups/ready" nil
                                              "agent=batch-seat&session=batch-session"))]
              (is (true? (get-in busy [:parked :withheld])))
              (is (true? (get-in busy [:followups :withheld])))
              (is (= parked-get (:parked busy)))
              (is (= followup-get (:followups busy))))
            (reg/clear-external-invoke! "batch-seat" "batch-busy")))))))

(deftest ready-batch-rejects-missing-or-non-list-seats
  (let [handler (http/make-handler {})]
    (doseq [payload [{} {:seats {:agent "not-a-list"}}]]
      (is (= {:ok false :error "seats-required"}
             (body (request handler :post "/api/alpha/ready/batch" payload nil)))))))

(deftest http-enqueue-withholds-while-busy-then-leases-emacs-shape-and-acks
  (let [path (str (System/getProperty "java.io.tmpdir")
                  "/futon3c-followup-http-test-" (random-uuid) ".edn")]
    (binding [queue/*path-override* path]
      (queue/clear!)
      (reg/reset-registry!)
      (reg/register-agent!
       {:agent-id {:id/value "codex-test" :id/type :continuity}
        :type :codex :session-id "session-exact" :capabilities [:edit]
        :invoke-fn (fn [_ _] {:result "ok"})})
      (let [handler (http/make-handler {})
            enqueue (request handler :post "/api/alpha/followups"
                             {:agent "codex-test" :session "session-exact"
                              :type "inbox-zero" :dedupe-key ["seat" "dirty-hash"]
                              :prompt "Inbox zero: review five files"} nil)]
        (is (= 200 (:status enqueue)))
        (reg/report-external-invoke! "codex-test" "e2e-test"
                                     {:status :invoking :session-id "session-exact"})
        (let [busy (body (request handler :get "/api/alpha/followups/ready" nil
                                         "agent=codex-test&session=session-exact"))]
          (is (true? (:withheld busy)))
          (is (empty? (:ready busy))))
        (reg/clear-external-invoke! "codex-test" "e2e-test")
        (let [ready (body (request handler :get "/api/alpha/followups/ready" nil
                                          "agent=codex-test&session=session-exact"))
              item (first (:ready ready))]
          ;; This is the exact payload shape consumed by agent-repl-park.el.
          (is (= "inbox-zero" (:type item)))
          (is (= "Inbox zero: review five files" (:prompt item)))
          (is (string? (:followup-id item)))
          (is (= {:ok true :acked true}
                 (body (request handler :post "/api/alpha/followups/ready/ack"
                                {:followup-id (:followup-id item)} nil)))))))))

(deftest ready-skips-stale-item-and-leases-next-current-item
  (let [path (str (System/getProperty "java.io.tmpdir")
                  "/futon3c-followup-http-test-" (random-uuid) ".edn")]
    (binding [queue/*path-override* path]
      (queue/clear!)
      (reg/reset-registry!)
      (reg/register-agent!
       {:agent-id {:id/value "codex-test" :id/type :continuity}
        :type :codex :session-id "session-exact" :capabilities [:edit]
        :invoke-fn (fn [_ _] {:result "ok"})})
      (let [handler (http/make-handler {})
            enqueue! #(request handler :post "/api/alpha/followups"
                               {:agent "codex-test" :session "session-exact"
                                :type "inbox-zero" :dedupe-key [%]
                                :prompt %} nil)]
        (enqueue! "stale")
        (enqueue! "current")
        (with-redefs [validity/validator
                      (fn [_] (fn [item] (= "current" (:prompt item))))]
          (let [ready (body (request handler :get "/api/alpha/followups/ready"
                                     nil "agent=codex-test&session=session-exact"))]
            (is (= "current" (:prompt (first (:ready ready)))))
            (is (= :stale (->> (:terminal (queue/snapshot)) vals first :reason)))))))))
