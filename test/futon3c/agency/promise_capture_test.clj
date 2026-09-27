(ns futon3c.agency.promise-capture-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.promise-capture :as capture]
            [futon3c.agency.promise-history :as history]
            [futon3c.agency.parked-on :as park]
            [futon3c.agency.followup-queue :as queue]))

(def ^:dynamic *evidence* nil)
(use-fixtures :each
  (fn [f]
    (let [p (java.io.File/createTempFile "capture-park" ".edn")
          q (java.io.File/createTempFile "capture-followup" ".edn")
          evidence (atom {:entries {} :order []})]
      (with-redefs-fn {#'park/store-path (constantly (str p))}
        #(binding [queue/*path-override* (str q) history/*backend* evidence
                   history/*heads* (atom {}) *evidence* evidence]
           (try
             (reset! @#'park/!parked nil)
             (reset! @#'queue/!state nil)
             (park/clear!) (queue/clear!) (f)
             (finally (history/await-writes! 5000)
                      (.delete p) (.delete q)
                      (reset! @#'park/!parked nil)
                      (reset! @#'queue/!state nil))))))))

(defn rows []
  (is (history/await-writes! 5000))
  (mapv #(get-in @*evidence* [:entries %]) (:order @*evidence*)))

(defn replay-edits [store entries]
  (reduce capture/apply-edits nil
          (for [entry entries change (get-in entry [:evidence/body :history/changes])
                :when (= store (:store change))]
            (:edits change))))

(defn covered! []
  ;; Derive coverage FROM actual snapshots at every lifecycle stage, rather than
  ;; maintaining a producer-side allowlist of record fields. All nested values,
  ;; derived indexes, FIFO positions and empty entries must match exactly.
  (let [entries (rows)]
    (doseq [[store live] [[:parked (park/snapshot)] [:followup (queue/snapshot)]]]
      (let [rebuilt (replay-edits store entries)]
        (is (= live rebuilt))
        (is (empty? (capture/coverage-issues store live rebuilt)))))))

(def rich-park
  {:agent "test" :session "session" :surface :bell :awaiting ["a" "b"]
   :payload {:arbitrary [nil #{:a :b} {:deep "payload"}]}
   :mode :background :deadline-ms 9999999 :timer-due-ms nil
   :budget {:resumes-left 1 :max-depth 4}
   :beneficiary "joe" :deadline "2026-10-01T00:00:00Z"
   :fulfilment-criterion {:kind :job-terminal-ok :job-id "a"}})

(deftest all-park-snapshot-fields-and-delivery-transitions
  (covered!)
  (let [id (:id (park/park! rich-park {:now-ms 1000}))]
    (covered!)
    (park/note-completion! "a" {:result {:arbitrary "result"}} {:now-ms 1001})
    (covered!)
    (park/note-completion! "b" {:ok true}
                           {:now-ms 1002 :resume! (fn [_] (park/ready-push! "test" "session" id "exact prompt" :background))})
    (covered!)
    (park/ready-lease-one! "test" "session" 2000 100)
    (covered!)
    (park/sweep-leased! {:now-ms 2200})
    (covered!)
    (park/ready-lease-one! "test" "session" 2300 100)
    (covered!)
    (park/rehydrate! {})
    (covered!)
    (park/ready-lease-one! "test" "session" 2400 100)
    (covered!)
    (park/ready-ack! id)
    (covered!)
    (is (every? (set (map :evidence/type (rows)))
                [:promise/ready-enqueued :promise/ready-leased :promise/ready-acked :promise/ready-requeued]))))

(deftest coalescing-budget-timer-and-deadline-coverage
  (let [r (assoc rich-park :awaiting ["one"] :budget {:resumes-left 0})
        first-id (:id (park/park! r {}))]
    (covered!)
    (is (= first-id (:id (park/park! (assoc r :payload "updated") {}))))
    (covered!)
    (park/note-completion! "one" {:ok true} {})
    (covered!))
  (park/park! (assoc rich-park :awaiting [] :timer-due-ms 20 :deadline-ms nil) {:now-ms 10})
  (covered!)
  (park/sweep-deadlines! {:now-ms 30 :resume! (fn [_])})
  (covered!)
  (park/park! (assoc rich-park :deadline-ms 40) {:now-ms 30})
  (covered!)
  (park/sweep-deadlines! {:now-ms 50 :resume! (fn [_])})
  (covered!))

(def followup {:agent "test" :session "session" :type :inbox-zero :prompt "exact prompt"
               :dedupe-key ["unique" 1] :metadata {:anything #{1 2}} :beneficiary "joe"})

(deftest all-followup-snapshot-fields-and-transitions
  (let [id (:id (queue/enqueue! followup))]
    (covered!)
    (queue/enqueue! (assoc followup :dedupe-key "second"))
    (covered!)
    (queue/lease-one! "test" "session" (constantly true))
    (covered!)
    (queue/ack! id)
    (covered!)
    (queue/lease-one! "test" "session" (constantly :stale))
    (covered!)
    (let [id (:id (queue/enqueue! (assoc followup :dedupe-key "cancel")))]
      (covered!) (queue/cancel! id :operator-released) (covered!))
    (queue/enqueue! (assoc followup :dedupe-key "expire"))
    (with-redefs-fn {#'queue/lease-ms -1}
      #(queue/lease-one! "test" "session" (constantly true)))
    (covered!)
    (queue/lease-one! "test" "session" (constantly true))
    (covered!)))

(deftest contiguous-chains-gap-and-legacy-reader
  (let [id (:id (park/park! (assoc rich-park :awaiting ["dep"]) {:now-ms 1000}))]
    (park/note-completion! "dep" {:ok true} {:now-ms 1000 :resume! (fn [_])})
    (let [entries (filterv #(= id (get-in % [:evidence/body :history/promise-id])) (rows))
          cut (vec (concat (take 1 entries) (drop 2 entries)))
          gap (first (history/check-chains cut))]
      (is (= [1 2 3 4] (mapv #(get-in % [:evidence/body :history/promise-sequence]) entries)))
      (is (empty? (history/check-chains entries)))
      (is (= {:promise-id id :reason :missing-transition :sequence 2
              :predecessor-id (:evidence/id (second entries))
              :predecessor-type :promise/dependency-terminated} gap))
      (println "deleted-record gap:" (pr-str gap))
      (is (= :incomplete-pre-repair-history
             (:reason (first (history/check-chains
                              [(update (first entries) :evidence/body dissoc :history/promise-sequence)])))))))
  (let [id (:id (queue/enqueue! followup))]
    (queue/lease-one! "test" "session" (constantly true))
    (queue/ack! id)
    (let [entries (filterv #(= id (get-in % [:evidence/body :history/promise-id])) (rows))]
      (is (= [1 2 3] (mapv #(get-in % [:evidence/body :history/promise-sequence]) entries)))
      (is (empty? (history/check-chains entries))))))

(deftest dummy-key-is-a-real-coverage-failure
  (let [live (park/snapshot)
        reconstructed (replay-edits :parked (rows))
        bad (assoc live :dummy-unrecorded {:new "field"})
        report (capture/coverage-issues :parked bad reconstructed)]
    (is (empty? (capture/coverage-issues :parked live reconstructed)))
    (is (= [{:reason :uncovered-snapshot-key :path [:dummy-unrecorded]}
            {:reason :snapshot-not-carried :path [:dummy-unrecorded]}] report))
    (println "dummy-key coverage refusal:" (pr-str report)))
  (is (= [:records "p" :new-field]
         (:path (first (capture/coverage-issues :parked
                         {:records {"p" {:new-field 1}}} {:records {"p" {}}}))))))

(deftest sequence-heads-survive-cache-loss
  (let [file (java.io.File/createTempFile "capture-heads" ".edn")
        cache @#'history/!chain-cache
        saved @cache
        rec {:id "durable-test" :agent "test" :session "session"}]
    (.delete file)
    (with-redefs-fn {#'history/chain-path (constantly (str file))}
      #(binding [history/*heads* nil]
         (try
           (reset! cache nil)
           (history/record! :promise/park-made rec 1000)
           (reset! cache nil) ; reconstruct metadata from its file, not evidence
           (history/record! :promise/dependency-terminated rec 1000)
           (let [entries (filterv (fn [r] (= "durable-test" (get-in r [:evidence/body :id]))) (rows))]
             (is (= [1 2] (mapv (fn [r] (get-in r [:evidence/body :history/promise-sequence])) entries)))
             (is (= (:evidence/id (first entries))
                    (get-in (second entries) [:evidence/body :history/predecessor :id]))))
           (finally (reset! cache saved) (.delete file)))))))
