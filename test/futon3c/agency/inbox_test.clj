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
