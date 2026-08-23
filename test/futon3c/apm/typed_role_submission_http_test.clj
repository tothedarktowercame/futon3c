(ns futon3c.apm.typed-role-submission-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.registry :as reg]
            [futon3c.apm.typed-role-submission :as submission]
            [futon3c.transport.http :as http])
  (:import (java.io ByteArrayInputStream)))

(defn request [method uri body]
  {:request-method method :uri uri :query-string nil
   :body (when body (ByteArrayInputStream. (.getBytes body "UTF-8")))})

(defn parsed [response]
  (json/parse-string (:body response) true))

(defn post-json [handler uri body]
  (handler (request :post uri (json/generate-string body))))

(defn await-job [handler job-id]
  (loop [remaining 200]
    (let [body (parsed (handler (request :get
                                         (str "/api/alpha/invoke/jobs/" job-id)
                                         nil)))
          state (get-in body [:job :state])]
      (if (or (contains? #{"done" "failed" "cancelled"} state)
              (zero? remaining))
        body
        (do (Thread/sleep 10) (recur (dec remaining)))))))

(defn with-isolated-invoke-ledger [f]
  (let [ledger (.toString (java.nio.file.Files/createTempFile
                           "typed-role-invokes" ".edn"
                           (make-array java.nio.file.attribute.FileAttribute 0)))]
    (java.nio.file.Files/deleteIfExists (java.nio.file.Paths/get ledger (make-array String 0)))
    (with-redefs [futon3c.transport.http/invoke-jobs-store-path (fn [] ledger)]
      (http/reset-invoke-jobs!)
      (try (f) (finally (http/reset-invoke-jobs!))))))

(deftest endpoint-returns-schema-and-rejects-malformed-before-accepting
  (let [root (.toString (java.nio.file.Files/createTempDirectory
                         "http-role-submission"
                         (make-array java.nio.file.attribute.FileAttribute 0)))
        authority {:submission/token "token-1" :dispatch/id "dispatch-1"
                   :agent-id "f30-guide" :frame-id "f30" :problem-id "m00A00"
                   :phase :guide-intervention-1 :role :guide}
        uri "/api/alpha/invoke/jobs/typed-job-1/submission"]
    (binding [submission/*submission-root* root]
      (is (:ok (submission/register! authority {:job-id "typed-job-1"})))
      (let [handler (http/make-handler {})
            schema (handler
                    (assoc (request :get uri nil) :query-string "token=token-1"))
            bad (handler
                 (request :post uri
                          (json/generate-string
                           {:token "token-1"
                            :payload {:command-own-exit 0 :outcome "complete"
                                      :failure-account [] :evidence {}}})))
            good (handler
                  (request :post uri
                           (json/generate-string
                            {:token "token-1"
                             :payload {:command-own-exit 0 :outcome "complete"
                                       :failure-account []
                                       :evidence {:channel-audit
                                                  {:direct-student-contact? false}}}})))]
        (is (= 200 (:status schema)))
        (is (= ["channel-audit"] (:evidence-required (parsed schema))))
        (is (= 422 (:status bad)))
        (is (= #{"channel-audit"} (set (:evidence/missing (parsed bad)))))
        (is (= 200 (:status good)))
        (is (= "submitted" (:status (parsed good))))))))

(deftest every-role-schema-completes-through-real-handler-with-exact-authority
  (with-isolated-invoke-ledger
   (fn []
    (let [root (.toString (java.nio.file.Files/createTempDirectory
                         "http-role-lifecycle"
                         (make-array java.nio.file.attribute.FileAttribute 0)))
        handler (http/make-handler {})
        active (atom nil)
        agent-id "typed-role-lifecycle-agent"
        phases (keys submission/evidence-required-by-phase)]
    (reg/register-agent!
     {:agent-id {:id/value agent-id :id/type :continuity}
      :type :codex :capabilities [:test]
      :invoke-fn
      (fn [prompt _session-id]
        (let [{:keys [job-id token phase]} @active
              evidence (into {}
                             (map (fn [field] [field true]))
                             (get submission/evidence-required-by-phase phase))]
          (is (.contains prompt job-id))
          (binding [submission/*submission-root* root]
            (is (:ok (submission/submit!
                      job-id token
                      {:command-own-exit 0 :outcome "complete"
                       :failure-account [] :evidence evidence}))))
          {:result "conversation is not the receipt" :session-id nil}))})
    (try
      (binding [submission/*submission-root* root]
        (doseq [phase phases]
          (let [dispatch-id (str "dispatch-" (name phase))
                request (submission/with-job-authority
                         (submission/prepare-request
                          {:dispatch/id dispatch-id :agent-id agent-id
                           :frame-id "f-test" :problem-id "p-test"
                           :phase phase :role :solver}))
                job-id (:submission/job-id request)
                token (:submission/token request)
                prompt (str "typed lifecycle " job-id)
                announce (parsed
                          (post-json handler "/api/alpha/invoke/announce"
                                     {:agent-id agent-id :prompt prompt
                                      :caller "test" :surface "test"
                                      :job-id job-id}))]
            (is (= job-id (:job-id announce)))
            (is (= 409
                   (:status
                    (post-json handler "/api/alpha/invoke/announce"
                               {:agent-id agent-id :prompt (str prompt " conflict")
                                :caller "test" :surface "test"
                                :job-id job-id}))))
            (is (:reused?
                 (parsed
                  (post-json handler "/api/alpha/invoke/announce"
                             {:agent-id agent-id :prompt prompt
                              :caller "test" :surface "test"
                              :job-id job-id}))))
            (is (:ok (submission/register! request {:job-id job-id})))
            (reset! active {:job-id job-id :token token :phase phase})
            ;; Simulate the post-restart process losing its in-memory invoke
            ;; cache. Activation must reconcile from the durable ledger.
            (when (= phase (first phases))
              (http/reset-invoke-jobs!)
              (is (= 409
                     (:status
                      (post-json handler "/api/alpha/invoke/activate"
                                 {:agent-id agent-id
                                  :prompt (str prompt " mutated")
                                  :caller "test" :surface "test"
                                  :job-id job-id})))))
            (let [activated (post-json handler "/api/alpha/invoke/activate"
                                       {:agent-id agent-id :prompt prompt
                                        :caller "test" :surface "test"
                                        :job-id job-id})
                  terminal (await-job handler job-id)]
              (is (= 202 (:status activated)))
              (is (= "done" (get-in terminal [:job :state])))
              (is (map? (submission/submitted job-id)))))))
      (finally
        (reg/unregister-agent! agent-id)))))))
