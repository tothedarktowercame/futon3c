(ns futon3c.nlp.classical-pipeline-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.backend]
            [futon3c.evidence.http-backend]
            [futon3c.nlp.classical-pipeline :as nlp]))

(deftest explicit-token-resolves
  (let [r (nlp/resolve-turn nlp/fixture-profiles
                            {:text "please clock this to M-foo"})]
    (is (= :resolved (:kind r)))
    (is (= {:campaign-id nil :mission-id "M-foo" :excursion-id nil}
           (:target r)))
    (is (= "explicit" (get-in r [:witness :rule])))))

(deftest term-similarity-proposes-only
  (let [r (nlp/resolve-turn nlp/fixture-profiles
                            {:text "the alpha beta gamma design needs the resolver"})]
    (is (= :proposal (:kind r)))
    (is (= {:campaign-id nil :mission-id "M-alpha-beta-gamma" :excursion-id nil}
           (:target r)))
    (is (= "term-similarity" (get-in r [:witness :rule])))
    (is (true? (get-in r [:witness :proposal-only])))))

(deftest off-topic-turn-proposes-nothing
  (is (nil? (nlp/resolve-turn nlp/fixture-profiles
                              {:text "make tea after lunch"}))))

(deftest affect-before-after-fixture
  (let [rows (mapv nlp/affect-row (take 5 nlp/fixture-turns))
        by-id (into {} (map (juxt :id identity) rows))]
    (testing "documented failures fixed"
      (is (= "-" (get-in by-id ["fixture-please" :after])))
      (is (= "-" (get-in by-id ["fixture-interesting" :after])))
      (is (= "-" (get-in by-id ["fixture-not-happy" :after]))))
    (testing "true joy and inspiration fire"
      (is (= "joy" (get-in by-id ["fixture-joy" :after])))
      (is (= "inspiration" (get-in by-id ["fixture-inspiration" :event-type]))))))

;; AR-43: a failed evidence read aborts the report at the CLI boundary — it
;; is never filtered into a silent zero-turn report.

(defn- stub-query-backend
  "A backend whose -query returns RESULT; used because protocol functions
  cannot be with-redefs'd reliably under direct linking."
  [result]
  (reify futon3c.evidence.backend/EvidenceBackend
    (-append [_ _] nil)
    (-get [_ _] nil)
    (-exists? [_ _] false)
    (-query [_ _] result)
    (-count [_ _] 0)
    (-forks-of [_ _] [])
    (-delete! [_ _] {:compacted 0})
    (-all [_] [])))

(deftest fetch-turns-aborts-on-a-failed-read
  (let [failure (with-redefs [futon3c.evidence.http-backend/make-http-backend
                              (fn [_] (stub-query-backend
                                       {:error/component :E-store :error/code :read-failed
                                        :error/kind :timeout :status nil
                                        :url "http://localhost:1/api/alpha/evidence"}))]
                  (try (nlp/fetch-turns {:base-url "http://localhost:1"
                                         :since "2026-09-25T00:00:00Z"})
                       ::no-throw
                       (catch clojure.lang.ExceptionInfo e e)))]
    (is (not= ::no-throw failure) "a failed read must throw, not read as zero turns")
    (is (= :evidence-read-failed (:failure (ex-data failure))))
    (is (= :read-failed (get-in (ex-data failure) [:read-error :error/code])))
    (is (= :timeout (get-in (ex-data failure) [:read-error :error/kind])))))

(deftest fetch-turns-genuine-empty-still-reads-as-empty
  (with-redefs [futon3c.evidence.http-backend/make-http-backend
                (fn [_] (stub-query-backend []))]
    (is (= [] (nlp/fetch-turns {:base-url "http://localhost:1"
                                :since "2026-09-25T00:00:00Z"})))))
