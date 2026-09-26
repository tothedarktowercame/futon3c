(ns futon3c.inbox-zero.followup-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.followup-queue :as queue]
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
