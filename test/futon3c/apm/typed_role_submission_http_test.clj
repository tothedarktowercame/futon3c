(ns futon3c.apm.typed-role-submission-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.typed-role-submission :as submission]
            [futon3c.transport.http :as http])
  (:import (java.io ByteArrayInputStream)))

(defn request [method uri body]
  {:request-method method :uri uri :query-string nil
   :body (when body (ByteArrayInputStream. (.getBytes body "UTF-8")))})

(defn parsed [response]
  (json/parse-string (:body response) true))

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
      (let [schema (http/extra-routes
                    (assoc (request :get uri nil) :query-string "token=token-1") {})
            bad (http/extra-routes
                 (request :post uri
                          (json/generate-string
                           {:token "token-1"
                            :payload {:command-own-exit 0 :outcome "complete"
                                      :failure-account [] :evidence {}}})) {})
            good (http/extra-routes
                  (request :post uri
                           (json/generate-string
                            {:token "token-1"
                             :payload {:command-own-exit 0 :outcome "complete"
                                       :failure-account []
                                       :evidence {:channel-audit
                                                  {:direct-student-contact? false}}}})) {})]
        (is (= 200 (:status schema)))
        (is (= ["channel-audit"] (:evidence-required (parsed schema))))
        (is (= 422 (:status bad)))
        (is (= #{"channel-audit"} (set (:evidence/missing (parsed bad)))))
        (is (= 200 (:status good)))
        (is (= "submitted" (:status (parsed good))))))))
