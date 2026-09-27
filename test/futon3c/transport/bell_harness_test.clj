(ns futon3c.transport.bell-harness-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.transport.http :as http]
            [futon3c.social.coordination-ledger :as ledger]
            [futon3c.agency.registry :as reg])
  (:import (java.nio.file Files)))

(defn request [payload]
  {:request-method :post :uri "/api/alpha/bell" :headers {}
   :body (java.io.ByteArrayInputStream. (.getBytes (json/generate-string payload) "UTF-8"))})

(deftest bell-persists-context-on-job-and-mesh-edge
  (let [root (.toFile (Files/createTempDirectory "bell-harness" (make-array java.nio.file.attribute.FileAttribute 0)))
        db (atom {:entries {} :order []})
        handler (http/make-handler {:evidence-store db})
        stamp {:kind "none" :basis "producer-context"}
        base {:agent-id "harness-test-seat" :caller "wm-full-loop" :prompt "no-op"}]
    (try
      (with-redefs-fn
        {#'ledger/*test-evidence-store* db
         #'http/invoke-jobs-store-path (constantly (str (io/file root "jobs.edn")))
         #'reg/agent-registered? (constantly true)
         #'http/inbox-agent? (constantly true)
         ;; Suppress delivery, not job creation, edge writes or GET projection.
         #'http/deliver-invoke-job-to-inbox! (fn [& _] "test-inbox")}
        (fn []
          (http/reset-invoke-jobs!)
          (doseq [payload [base (assoc base :harness stamp)]]
            (let [response (handler (request payload))
                  id (:job-id (json/parse-string (:body response) true))
                  get-response (handler {:request-method :get :uri (str "/api/alpha/invoke/jobs/" id)})
                  job (:job (json/parse-string (:body get-response) true))
                  edge (first (filter #(= id (get-in % [:evidence/body :edge/id])) (vals (:entries @db))))]
              (is (= 202 (:status response)))
              (is (= 200 (:status get-response)))
              (is (some? edge))
              (is (= (contains? payload :harness) (contains? job :harness)))
              (is (= (contains? payload :harness) (contains? edge :evidence/harness)))
              (when (contains? payload :harness)
                (is (= stamp (:harness job)))
                (is (= {:kind :none :basis :producer-context} (:evidence/harness edge))))))
          (let [jobs (:jobs (#'http/ensure-invoke-jobs-ledger!))
                id (:job-id (first (filter #(contains? % :harness) (vals jobs))))
                before @db
                response (handler (request (assoc base :job-id id)))]
            (is (= 409 (:status response)))
            (is (= "harness-conflict" (:err (json/parse-string (:body response) true))))
            (is (= before @db)))
          (doseq [bad [nil {:kind "zai" :basis "producer-context"}
                       {:kind "war-machine" :basis "producer-context"}
                       {:kind "unknown" :basis "producer-context"}
                       (assoc stamp :extra true) (assoc stamp :kind "typo")]]
            (let [before (#'http/ensure-invoke-jobs-ledger!) before-edges @db
                  response (handler (request (assoc base :harness bad)))
                  parsed (json/parse-string (:body response) true)]
              (is (= 400 (:status response)))
              (is (= "invalid-harness" (:err parsed)))
              (is (string? (:reason parsed)))
              (is (= before (#'http/ensure-invoke-jobs-ledger!)))
              (is (= before-edges @db))))
          (http/reset-invoke-jobs!)))
      (finally (doseq [f (reverse (file-seq root))] (io/delete-file f))))))
