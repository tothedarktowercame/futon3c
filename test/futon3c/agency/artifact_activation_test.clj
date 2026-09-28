(ns futon3c.agency.artifact-activation-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.artifact-activation :as activation]
            [futon3c.social.shapes :as shapes])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def observed-at "2026-09-28T21:00:00Z")
(def descriptor
  {:name :futon3a/notions-search
   :model "sentence-transformers/all-MiniLM-L6-v2"
   :script-sha256 "script-a"
   :index-sha256 "index-a"})
(def artifact {:kind :invoke-job :id "invoke-work-1" :observed-at observed-at})
(def hits
  [{:id "agency/state-atomicity" :title "Atomic" :score 0.8}
   {:id "social/idempotent-handoff" :title "Handoff" :score 0.7}
   {:id "象/限定随论" :title "限定随论" :score 0.6}])

(deftest exact-citations-are-not-weak
  (let [known (mapv :id hits)]
    (is (= ["agency/state-atomicity"]
           (activation/cited-ids "Use ~agency/state-atomicity." known)))
    (is (= ["social/idempotent-handoff"]
           (activation/cited-ids "Use `social/idempotent-handoff`." known)))
    (is (= ["agency/state-atomicity"]
           (activation/cited-ids "[atomic](agency/state-atomicity)" known)))
    (is (= ["象/限定随论"]
           (activation/cited-ids "遵守 象/限定随论。" known)))
    (is (empty? (activation/cited-ids "state-atomicity" known)))
    (let [record (activation/activation-record
                  artifact "~agency/state-atomicity and 象/限定随论" descriptor hits)]
      (is (shapes/valid? shapes/EvidenceEntry record))
      (is (= ["agency/state-atomicity" "象/限定随论"]
             (get-in record [:evidence/body :cited])))
      (is (= ["social/idempotent-handoff"]
             (get-in record [:evidence/body :weak]))))))

(defn- fake-store [] (atom {}))

(defmacro with-fake-recording [store & body]
  `(binding [activation/*descriptor-fn* (constantly descriptor)
             activation/*search-fn* (fn [_# _#] hits)
             activation/*get-fn* (fn [_# id#] (get @~store id#))
             activation/*append-fn*
             (fn [_# entry#]
               (swap! ~store assoc (:evidence/id entry#) entry#)
               {:ok true :entry entry#})]
     ~@body))

(deftest record-is-idempotent-and-input-sensitive
  (let [store (fake-store)]
    (with-fake-recording store
      (let [first-result (activation/record! store artifact "packet")
            second-result (activation/record! store artifact "packet")]
        (is (= :recorded (:status first-result)))
        (is (= :existing (:status second-result)))
        (is (= 1 (count @store)))
        (activation/record! store artifact "changed packet")
        (is (= 2 (count @store)))))
    (binding [activation/*descriptor-fn*
              (constantly (assoc descriptor :index-sha256 "index-b"))
              activation/*search-fn* (fn [_ _] hits)
              activation/*get-fn* (fn [_ id] (get @store id))
              activation/*append-fn* (fn [_ entry]
                                       (swap! store assoc (:evidence/id entry) entry)
                                       {:ok true :entry entry})]
      (activation/record! store artifact "packet")
      (is (= 3 (count @store)) "a different index produces a different id"))))

(deftest search-failure-is-a-record
  (let [store (fake-store)]
    (binding [activation/*descriptor-fn* (constantly descriptor)
              activation/*search-fn*
              (fn [_ _] (throw (ex-info "offline" {:error/code :search-offline})))
              activation/*get-fn* (fn [_ id] (get @store id))
              activation/*append-fn* (fn [_ entry]
                                       (swap! store assoc (:evidence/id entry) entry)
                                       {:ok true :entry entry})]
      (is (= :recorded (:status (activation/record! store artifact "packet"))))
      (let [entry (first (vals @store))]
        (is (= {:reason :search-offline :message "offline"}
               (get-in entry [:evidence/body :error])))
        (is (empty? (get-in entry [:evidence/body :hits])))))))

(deftest submit-does-not-wait-for-search
  (let [entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        appended (promise)
        started (System/nanoTime)]
    (binding [activation/*descriptor-fn* (constantly descriptor)
              activation/*search-fn* (fn [_ _]
                                       (.countDown entered)
                                       (.await release 2 TimeUnit/SECONDS)
                                       hits)
              activation/*get-fn* (fn [_ _] nil)
              activation/*append-fn* (fn [_ entry]
                                       (deliver appended entry)
                                       {:ok true :entry entry})]
      (is (= :submitted (activation/submit-work! nil artifact "packet")))
      (is (< (/ (- (System/nanoTime) started) 1000000.0) 100.0))
      (is (.await entered 1 TimeUnit/SECONDS))
      (is (not (realized? appended)))
      (.countDown release)
      (is (some? (deref appended 1000 nil))))))

(deftest missing-record-detects-the-bad-case
  (let [success (activation/activation-record artifact "packet" descriptor hits)
        failure (activation/activation-record
                 (assoc artifact :id "invoke-work-2") "packet" descriptor
                 {:error {:reason :search-failed :message "failed"}})]
    (is (= [{:job-id "invoke-missing" :reason :activation-missing}]
           (activation/missing-activations
            ["invoke-work-1" "invoke-work-2" "invoke-missing"]
            [success failure])))))

(deftest empty-search-is-a-typed-failure-not-an-empty-success
  (doseq [result [nil []]]
    (let [appended (atom nil)]
      (binding [activation/*search-fn* (fn [_ _] result)
                activation/*descriptor-fn* (constantly descriptor)
                activation/*get-fn* (fn [_ _] nil)
                activation/*append-fn* (fn [_ entry] (reset! appended entry) {:ok true :entry entry})]
        (activation/record! nil artifact "packet"))
      (is (= :search-returned-nothing
             (get-in @appended [:evidence/body :error :reason]))))))

(deftest real-descriptor-hashes-files-and-caches
  ;; Every other test fakes *descriptor-fn*; this one runs the real hashing.
  (let [script (java.io.File/createTempFile "notions" ".py")
        index (java.io.File/createTempFile "index" ".json")]
    (spit script "print(1)")
    (spit index "[]")
    (with-redefs [activation/script-file (constantly script)
                  activation/index-file (constantly index)]
      (let [d1 (activation/retrieval-descriptor)
            d2 (activation/retrieval-descriptor)]
        (is (re-matches #"[0-9a-f]{64}" (:index-sha256 d1)))
        (is (not= (:script-sha256 d1) (:index-sha256 d1)))
        (is (= d1 d2))))))
