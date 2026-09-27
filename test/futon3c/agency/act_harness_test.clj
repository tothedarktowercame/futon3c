(ns futon3c.agency.act-harness-test
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.act-harness :as harness]
            [futon3c.agency.grant-record :as grant]
            [futon3c.agency.incident-clearance :as clearance]
            [futon3c.agency.rule-record :as rule]))

(def lab "holes/labs/M-象-2000/")
(def rule-request
  (edn/read-string (slurp (str lab "P13a-requisition-rule.edn"))))
(def grant-request
  (edn/read-string (slurp (str lab "P3-1-grant-1620.edn"))))
(def grant-context
  (edn/read-string (slurp (str lab "P3-1-source-fixture.edn"))))
(def clearance-request
  (edn/read-string (slurp (str lab "P14-clearance-record.edn"))))
(def clearance-context
  (json/read-str (slurp (str lab "P14-validation-context.json")) :key-fn keyword))

(def payloads
  [["rule" #(rule/payload rule-request %)]
   ["grant" #(grant/payload grant-request grant-context %)]
   ["clearance" #(clearance/payload clearance-request clearance-context %)]])

(defn reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest every-cli-default-arity-stamps-plain-session
  (doseq [[payload source-ref]
          [[(rule/payload rule-request) "cli:futon3c.agency.rule-record"]
           [(grant/payload grant-request grant-context) "cli:futon3c.agency.grant-record"]
           [(clearance/payload clearance-request clearance-context)
            "cli:futon3c.agency.incident-clearance"]]]
    (is (= {:kind :none :basis :producer-context :source-ref source-ref}
           (get-in payload [:hx/props :act/harness])))))

(deftest every-cli-payload-stamps-default-and-war-machine
  (doseq [[label build] payloads]
    (testing label
      (let [default (:act/harness (:hx/props (build (harness/plain (str "cli:" label)))))
            wm {:kind :war-machine :basis :producer-context
                :execution-id (str label "-run") :source-ref (str "cli:" label)}]
        (is (= {:kind :none :basis :producer-context :source-ref (str "cli:" label)}
               default))
        (is (= wm (:act/harness (:hx/props (build wm)))))))))

(deftest every-cli-payload-refuses-invalid-harness
  (doseq [[label build] payloads]
    (testing label
      (is (= :missing-execution-id
             (reason #(build {:kind :war-machine :basis :producer-context}))))
      (is (= :zai-harness-not-deployed
             (reason #(build {:kind :zai :basis :producer-context})))))))

(deftest cli-flag-parser-requires-valid-execution-context
  (is (= {:kind :war-machine :basis :producer-context
          :source-ref "cli:test" :execution-id "run-2"}
         (:harness (harness/parse-cli
                    ["--write" "--harness-kind" "war-machine"
                     "--harness-execution-id" "run-2" "record.edn"]
                    "cli:test" "usage"))))
  (is (= :missing-execution-id
         (reason #(harness/parse-cli ["--harness-kind" "war-machine" "record.edn"]
                                    "cli:test" "usage"))))
  (is (= :zai-harness-not-deployed
         (reason #(harness/parse-cli ["--harness-kind" "zai" "record.edn"]
                                    "cli:test" "usage")))))

(deftest old-stored-acts-without-harness-remain-valid
  (is (= (:record rule-request) (rule/validate! (:record rule-request))))
  (is (= (:record grant-request)
         (grant/validate! (:record grant-request) grant-context)))
  (is (= (:record clearance-request)
         (clearance/validate! (:record clearance-request) clearance-context))))
