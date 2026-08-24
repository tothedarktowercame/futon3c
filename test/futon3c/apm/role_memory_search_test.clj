(ns futon3c.apm.role-memory-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.role-memory-search :as sut]
            [futon3c.apm.typed-role-submission :as submission]))

(defn temp-dir [prefix]
  (.toString
   (java.nio.file.Files/createTempDirectory
    prefix (make-array java.nio.file.attribute.FileAttribute 0))))

(defn register! [root role job-id]
  (binding [submission/*submission-root* root]
    (submission/register!
     {:submission/token "token" :dispatch/id "dispatch"
      :agent-id "seat" :frame-id "f29" :problem-id "a01J05"
      :phase (if (= role :student) :student-attempt-1 :promote-solver)
      :role role}
     {:job-id job-id})))

(deftest authenticated-search-persists-content-addressed-open-corpus-receipt
  (let [authority-root (temp-dir "role-search-authority")
        receipt-root (temp-dir "role-search-receipts")
        calls (atom [])
        search-result {:ok true :trace-id "ignored" :index-as-of "index-7"
                       :content-matches [{:memory/id "outside-snapshot-memory"}]
                       :candidates [{:pattern-id "math/canonical-pattern"}]}]
    (is (:ok (register! authority-root :student "student-job")))
    (binding [submission/*submission-root* authority-root
              sut/*receipt-root* receipt-root
              sut/*search-fn* (fn [ctx query opts]
                                (swap! calls conj [ctx query opts])
                                search-result)]
      (let [first-result (sut/search! "student-job" "token" "Gauss Lucas" 50)
            replay (sut/search! "student-job" "token" "Gauss Lucas" 50)
            receipt (:receipt first-result)]
        (is (= :recorded (:status first-result)))
        (is (= :already-recorded (:status replay)))
        (is (= :reviewed-mathematics (:corpus/scope receipt)))
        (is (= :student (:role receipt)))
        (is (= 10 (:limit receipt)))
        (is (= ["math/canonical-pattern" "outside-snapshot-memory"]
               (:result-ids receipt)))
        (is (= receipt (sut/receipt (:receipt/id receipt))))
        (is (= (first @calls) (second @calls))
            "read-only effect replay uses identical deterministic trace identity")))))

(deftest unauthorized-role-and-narrative-only-query-are-refused
  (let [authority-root (temp-dir "role-search-auth-refusal")
        receipt-root (temp-dir "role-search-receipt-refusal")]
    (is (:ok (register! authority-root :guide "guide-job")))
    (binding [submission/*submission-root* authority-root
              sut/*receipt-root* receipt-root
              sut/*search-fn* (fn [& _] (throw (ex-info "must not run" {})))]
      (testing "controller authority, not caller assertion, owns capability"
        (is (= :role-memory-search-not-authorized
               (:error/code (sut/search! "guide-job" "token" "query" 3)))))
      (testing "a narrated query without an executed receipt has no record"
        (is (nil? (sut/receipt "self-reported-query")))))))

(deftest exactly-the-three-contract-roles-have-search-authority
  (doseq [role [:student :scribe :promotion-proctor]]
    (let [authority-root (temp-dir (str "role-search-" (name role)))
          receipt-root (temp-dir "role-search-role-receipt")
          job-id (str (name role) "-job")]
      (is (:ok (register! authority-root role job-id)))
      (binding [submission/*submission-root* authority-root
                sut/*receipt-root* receipt-root
                sut/*search-fn* (fn [& _]
                                  {:ok true :content-matches [] :candidates []})]
        (is (:ok (sut/search! job-id "token" "canonical namespace" 1))
            (name role))))))
