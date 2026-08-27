(ns futon3c.apm.role-memory-search-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
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
        (is (= #{"math/canonical-pattern" "outside-snapshot-memory"}
               (sut/recorded-result-ids-for-job "student-job")))
        (is (= #{"math/canonical-pattern" "outside-snapshot-memory"}
               (sut/recorded-surfaced-ids-for-job "student-job")))
        (is (= [receipt] (sut/recorded-receipts-for-job "student-job")))
        (is (= [] (sut/recorded-receipts-for-job "another-job")))
        (is (= #{} (sut/recorded-result-ids-for-job "another-job")))
        (is (:ok (sut/validate-claims
                  {:job-id "student-job" :dispatch/id "dispatch" :role :student}
                  [(:receipt/id receipt)])))
        (is (= (first @calls) (second @calls))
            "read-only effect replay uses identical deterministic trace identity")))))

(deftest receipt-surfaced-ids-includes-explicit-linked-identifiers
  (is (= #{"primary" "memory" "mission" "subject" "pattern" "e-round"
           "candidate" "support"}
         (sut/receipt-surfaced-ids
          {:result-ids ["primary"]
           :content-matches [{:memory/id "memory"
                              :memory/hyperedge-id "hx:memory.assert.e-round.pattern"
                              :memory/mission-ids ["mission"]
                              :memory/subject-ids ["subject"]
                              :memory/pattern-ids ["pattern"]}]
           :candidates [{:pattern-id "candidate"
                         :memory-support [{:memory-id "support"}]}]}))))

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

(deftest f29-fragmented-namespace-is-killed-by-canonical-search-accounting
  (let [{:keys [search-result-pattern-id f29-proposed-pattern-id expected-error]}
        (edn/read-string
         (slurp "test/fixtures/apm/f29-fragmented-pattern-namespace.edn"))
        receipt {:candidates [{:pattern-id search-result-pattern-id}]}
        evidence {:lanes [{:pattern-ids [f29-proposed-pattern-id]}]}
        refused (sut/validate-pattern-accounting [receipt] evidence)
        reused (sut/validate-pattern-accounting
                [receipt] {:lanes [{:pattern-ids [search-result-pattern-id]}]})
        justified (sut/validate-pattern-accounting
                   [receipt]
                   (assoc evidence :new-pattern-rationales
                          {f29-proposed-pattern-id
                           "Distinct hypotheses and conclusion; not an alias."}))]
    (is (= expected-error (:error/code refused)))
    (is (= #{f29-proposed-pattern-id} (:unaccounted-pattern-ids refused)))
    (is (:ok reused))
    (is (:ok justified))))

(deftest f35-keywordized-rationale-keys-still-account-for-new-patterns
  ;; The typed JSON submission keywordizes map keys, so the promotion
  ;; Proctor's :new-pattern-rationales arrive keyed by namespaced keywords
  ;; while :pattern-ids stay strings. Before this normalization every guide
  ;; approval naming a pattern outside the reviewer's FTS hits was refused
  ;; (f33 and f35 guide-intervention-1, 2026-08-25).
  (let [receipt {:candidates [{:pattern-id "math-informal/local-to-global"}]}
        proposed "math-formalization/compact-thickening-upgrades-pointwise-analyticity"
        evidence {:reviews [{:pattern-ids [proposed]}]
                  :new-pattern-rationales
                  {(keyword proposed) "No canonical pattern covers the thickening step."}}
        accounted (sut/validate-pattern-accounting [receipt] evidence)
        still-refused (sut/validate-pattern-accounting
                       [receipt] (assoc evidence :new-pattern-rationales
                                        {(keyword proposed) ""}))]
    (is (:ok accounted))
    (is (= #{proposed} (:proposed-pattern-ids accounted)))
    (is (= :canonical-pattern-reuse-unaccounted (:error/code still-refused)))))

(defn register-holdout! [root job-id withheld]
  (binding [submission/*submission-root* root]
    (submission/register!
     {:submission/token "token" :dispatch/id "dispatch"
      :agent-id "seat" :frame-id "f48" :problem-id "a98A03"
      :phase :student-attempt-1 :role :student
      :shelf/holdout :same-problem :shelf/withheld-ids withheld}
     {:job-id job-id})))

(deftest search-never-serves-a-withheld-memory
  (testing "f48/a1: the shelf and cascade withheld an id the search returned"
    (let [authority-root (temp-dir "role-search-holdout-authority")
          receipt-root (temp-dir "role-search-holdout-receipts")
          withheld "e-apm-promotion-9b8d0aec504ee645aa3130fc7768738b"
          search-result {:ok true :index-as-of "index-9"
                         :content-matches [{:memory/id withheld}
                                           {:memory/id "unheld-memory"}]
                         :candidates [{:pattern-id withheld}
                                      {:pattern-id "math/unheld-pattern"}]}]
      (is (:ok (register-holdout! authority-root "held-job" [withheld])))
      (binding [submission/*submission-root* authority-root
                sut/*receipt-root* receipt-root
                sut/*search-fn* (fn [_ _ _] search-result)]
        (let [receipt (:receipt (sut/search! "held-job" "token" "Cantor" 10))]
          (is (= ["math/unheld-pattern" "unheld-memory"] (:result-ids receipt)))
          (is (= [withheld] (:holdout/excluded-ids receipt)))
          (is (= :same-problem (:shelf/holdout receipt)))
          (is (= 1 (:holdout/withheld-count receipt)))
          (is (not (contains? (sut/recorded-surfaced-ids-for-job "held-job")
                              withheld))))))))

(deftest search-without-a-holdout-is-unfiltered
  (testing "attempts 2 and 3 carry no holdout and must keep every result"
    (let [authority-root (temp-dir "role-search-nofilter-authority")
          receipt-root (temp-dir "role-search-nofilter-receipts")
          search-result {:ok true :index-as-of "index-9"
                         :content-matches [{:memory/id "any-memory"}]
                         :candidates []}]
      (is (:ok (register! authority-root :student "open-job")))
      (binding [submission/*submission-root* authority-root
                sut/*receipt-root* receipt-root
                sut/*search-fn* (fn [_ _ _] search-result)]
        (let [receipt (:receipt (sut/search! "open-job" "token" "Cantor" 10))]
          (is (= ["any-memory"] (:result-ids receipt)))
          (is (= [] (:holdout/excluded-ids receipt)))
          (is (nil? (:shelf/holdout receipt))))))))
