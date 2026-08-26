(ns futon3c.agency.inbox-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.turn-queue :as turn-queue]
            [futon3c.transport.http :as http]))

(defn- post-bell
  [payload]
  (#'http/handle-bell
   {:body (json/generate-string payload)}
   {}))

(defn- response-body
  [response]
  (json/parse-string (:body response) true))

(defn- request
  ([handler method uri]
   (request handler method uri {}))
  ([handler method uri payload]
   (handler {:request-method method
             :uri uri
             :body (json/generate-string payload)})))

(defn- register-inbox-agent!
  [agent-id]
  (reg/register-agent!
   {:agent-id agent-id
    :type :mock
    :delivery-mode :inbox
    :invoke-fn (fn [& _] (throw (ex-info "pull-only invoke-fn ran" {})))
    :capabilities [:invoke]}))

(defn- set-job-created-at!
  [job-id instant]
  (#'http/update-invoke-jobs-ledger!
   (fn [ledger]
     (assoc-in ledger [:jobs job-id :created-at] (str instant)))))

(use-fixtures
  :each
  (fn [f]
    (let [ledger (java.io.File/createTempFile "agency-inbox-test" ".edn")]
      (.delete ledger)
      (with-redefs-fn {#'http/invoke-jobs-store-path
                       (fn [] (.getAbsolutePath ledger))
                       #'turn-queue/drainer-v2-enabled?
                       (constantly false)}
        (fn []
          (reg/reset-registry!)
          (http/reset-invoke-jobs!)
          (try
            (f)
            (finally
              (reg/reset-registry!)
              (http/reset-invoke-jobs!)
              (io/delete-file ledger true))))))))

(deftest pull-only-bell-is-atomically-delivered-without-invocation
  (let [root (System/getenv "FUTON3C_AGENCY_INBOX_DIR")
        suffix (str (java.util.UUID/randomUUID))
        inbox-id (str "pull-seat-" suffix)
        push-id (str "push-seat-" suffix)
        push-called (promise)
        prompt "full prompt, including\na second line"]
    (is (some? root)
        "run this test with FUTON3C_AGENCY_INBOX_DIR set to a temporary directory")
    (reg/register-agent!
     {:agent-id inbox-id
      :type :mock
      :delivery-mode :inbox
      :invoke-fn (fn [& _] (throw (ex-info "pull-only invoke-fn ran" {})))
      :capabilities [:invoke]})
    (is (= :pull-only-agent
           (get-in (reg/invoke-agent! inbox-id "must not run")
                   [:error :error/code])))
    (is (= :inbox
           (get-in (reg/registry-status) [:agents inbox-id :delivery-mode])))
    (reg/register-agent!
     {:agent-id push-id
      :type :mock
      :invoke-fn (fn [& _]
                   (deliver push-called true)
                   {:result "ok" :session-id nil})
      :capabilities [:invoke]})
    (let [response (post-bell {:agent-id inbox-id
                               :caller "test-caller"
                               :prompt prompt
                               :type "request"})
          body (response-body response)
          job-id (:job-id body)
          inbox-file (io/file root inbox-id (str job-id ".json"))
          payload (json/parse-string (slurp inbox-file) true)
          job (#'http/get-invoke-job job-id)]
      (is (= 202 (:status response)))
      (is (= "delivered" (:state body)))
      (is (= (.getAbsolutePath inbox-file) (:inbox-path body)))
      (is (.isFile inbox-file))
      (is (= prompt (:prompt payload)))
      (is (= inbox-id (:agent-id payload)))
      (is (= "test-caller" (:from payload)))
      (is (= (str "/api/alpha/invoke/jobs/" job-id "/ack") (:ack-url payload)))
      (is (= "delivered" (:state job)))
      (is (false? (#'http/invoke-job-terminal-state? (:state job)))))
    (let [job-id (#'http/enqueue-auto-bellback!
                  {:caller inbox-id
                   :bell-job-id (str "auto-bellback-" suffix)
                   :prompt "automated completion"
                   :reply-to "original-job"})
          inbox-file (io/file root inbox-id (str job-id ".json"))
          payload (json/parse-string (slurp inbox-file) true)]
      (is (= "delivered" (:state (#'http/get-invoke-job job-id))))
      (is (= "automated completion" (:prompt payload)))
      (is (= "original-job" (:in-reply-to payload))))
    (post-bell {:agent-id push-id :caller "test-caller" :prompt "push prompt"})
    (is (= true (deref push-called 5000 false))
        "default push delivery still invokes the registered invoke-fn")))

(deftest inbox-ack-is-the-single-consumption-receipt
  (let [root (System/getenv "FUTON3C_AGENCY_INBOX_DIR")
        suffix (str (java.util.UUID/randomUUID))
        agent-id (str "ack-seat-" suffix)
        handler (http/make-handler {})]
    (register-inbox-agent! agent-id)
    (let [missing (request handler :post
                           "/api/alpha/invoke/jobs/missing-inbox-job/ack")]
      (is (= 404 (:status missing)))
      (is (= "invoke-job-not-found" (:error (response-body missing)))))
    (let [first-bell (response-body
                      (post-bell {:agent-id agent-id
                                  :caller "test-caller"
                                  :prompt "old unread delivery"}))
          first-id (:job-id first-bell)
          first-file (io/file root agent-id (str first-id ".json"))]
      (set-job-created-at! first-id (.minusSeconds (java.time.Instant/now) 120))
      (let [status (get-in (reg/registry-status) [:agents agent-id])]
        (is (= 1 (:unconsumed-count status)))
        (is (<= 120000 (:oldest-unconsumed-age-ms status))))
      (let [ack-response (request handler :post
                                  (str "/api/alpha/invoke/jobs/" first-id "/ack")
                                  {:note "read by test" :result "consumed"})
            ack-body (response-body ack-response)
            job (#'http/get-invoke-job first-id)
            original-receipt (:delivery job)
            consumed-file (io/file root agent-id "consumed" (str first-id ".json"))]
        (is (= 200 (:status ack-response)))
        (is (= "done" (:state ack-body)))
        (is (= "done" (:state job)))
        (is (= "inbox-consumed-ack" (get-in job [:delivery :note])))
        (is (some #(and (= "acked" (:type %))
                        (= "read by test" (:note %)))
                  (:events job)))
        (is (false? (.exists first-file)))
        (is (.isFile consumed-file))
        (is (= 0 (get-in (reg/registry-status)
                         [:agents agent-id :unconsumed-count])))
        (let [second (request handler :post
                              (str "/api/alpha/invoke/jobs/" first-id "/ack"))]
          (is (= 409 (:status second)))
          (is (= "invoke-job-not-delivered" (:error (response-body second))))
          (is (= original-receipt
                 (:delivery (#'http/get-invoke-job first-id))))))
      (let [push-job (#'http/create-invoke-job!
                      {:requested-job-id (str "push-job-" suffix)
                       :agent-id "push-seat"
                       :prompt "not an inbox delivery"
                       :caller "test-caller"
                       :surface "bell"})
            response (request handler :post
                              (str "/api/alpha/invoke/jobs/" push-job "/ack"))]
        (is (= 409 (:status response)))
        (is (= "not-inbox-job" (:error (response-body response)))))
      (let [older (response-body
                   (post-bell {:agent-id agent-id
                               :caller "test-caller"
                               :prompt "older backlog item"}))
            newer (response-body
                   (post-bell {:agent-id agent-id
                               :caller "test-caller"
                               :prompt "newer backlog item"}))
            older-id (:job-id older)
            newer-id (:job-id newer)]
        (set-job-created-at! older-id (.minusSeconds (java.time.Instant/now) 180))
        (set-job-created-at! newer-id (.minusSeconds (java.time.Instant/now) 30))
        (is (= 200 (:status (request handler :post
                                    (str "/api/alpha/invoke/jobs/" newer-id "/ack")))))
        (let [status (get-in (reg/registry-status) [:agents agent-id])
              health (response-body (request handler :get "/health"))]
          (is (= 1 (:unconsumed-count status)))
          (is (<= 180000 (:oldest-unconsumed-age-ms status)))
          (is (= 1 (:unconsumed-count health)))
          (is (= agent-id (:oldest-unconsumed-agent-id health)))
          (is (<= 180000 (:oldest-unconsumed-age-ms health))))))))
