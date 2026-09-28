(ns futon3c.agency.promise-history-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.promise-history :as history]
            [futon3c.agency.atomic-file :as atomic-file]
            [futon3c.agency.promise-outcome]
            [futon3c.agency.parked-on :as park]
            [futon3c.agency.followup-queue :as queue]
            [futon3c.evidence.futon1b-backend :as f1b]))

(def ^:dynamic *evidence* nil)
(use-fixtures :each
  (fn [f]
    (let [p (java.io.File/createTempFile "p2a-park" ".edn")
          q (java.io.File/createTempFile "p2a-followup" ".edn")
          store (atom {:entries {} :order []})]
      (with-redefs-fn {#'park/store-path (constantly (str p))}
        #(binding [queue/*path-override* (str q) history/*backend* store history/*heads* (atom {}) *evidence* store]
           (try (park/clear!) (queue/clear!)
                (history/await-writes! 5000)
                (reset! store {:entries {} :order []})
                (f)
                (finally (history/await-writes! 5000)
                         (.delete p) (.delete q)
                         (reset! @#'park/!parked nil)
                         (reset! @#'queue/!state nil))))))))

(def request {:agent "test-p2a" :session "test-p2a" :awaiting ["dep"]
              :beneficiary "joe" :deadline "2026-10-01T00:00:00Z"
              :fulfilment-criterion {:kind :job-terminal-ok :job-id "dep"}})
(defn rows []
  (is (history/await-writes! 5000))
  (mapv #(get-in @*evidence* [:entries %]) (:order @*evidence*)))
(defn types [] (mapv :evidence/type (rows)))

(deftest four-distinct-transitions-not-fulfilment
  (let [wakes (atom [])
        id (:id (park/park! request {:now-ms 1000}))]
    (park/note-completion! "dep" {:ok true} {:now-ms 2000 :resume! #(swap! wakes conj %)})
    (park/note-completion! "dep" {:ok true} {:now-ms 3000 :resume! #(swap! wakes conj %)})
    (let [entries (rows)]
      (is (= [:promise/park-made :promise/dependency-terminated :promise/woken :promise/released]
             (mapv :evidence/type entries)))
      (is (not-any? #(re-find #"fulfil|kept" (str (:evidence/type %))) entries))
      (is (every? #(= :harness (get-in % [:evidence/origin :kind])) entries))
      (is (every? #(= "test-p2a" (:evidence/author %)) entries))
      (is (= 4 (count (set (map :evidence/id entries)))))
      (is (apply < (map #(get-in % [:evidence/body :history/sequence]) entries)))
      (is (= 1 (count @wakes)))
      (is (every? #(= id (get-in % [:evidence/body :id])) entries))
      (is (every? #(= "joe" (get-in % [:evidence/body :beneficiary])) entries))
      (is (every? #(= ["dep"] (get-in % [:evidence/body :awaiting])) entries))
      ;; Writer executes later; evidence/at must retain the injected event clock.
      (is (= ["1970-01-01T00:00:01Z" "1970-01-01T00:00:02Z"
              "1970-01-01T00:00:02Z" "1970-01-01T00:00:02Z"]
             (mapv :evidence/at entries))))))

(deftest budget-exhaustion-is-visible
  (park/park! (assoc request :budget {:resumes-left 0}) {})
  (park/note-completion! "dep" {:ok true} {:resume! #(throw (ex-info "Unexpected wake" %))})
  (is (= [:promise/park-made :promise/dependency-terminated :promise/budget-exhausted] (types)))
  (is (empty? (:records (park/snapshot)))))

(deftest deadline-and-immediate-wakes
  (park/park! (assoc request :deadline-ms 20) {:now-ms 10})
  (park/sweep-deadlines! {:now-ms 30 :resume! (fn [_])})
  (is (= [:promise/park-made :promise/deadline-expired :promise/woken :promise/released] (types)))
  (park/park! (assoc request :awaiting []) {:resume! (fn [_])})
  (is (= [:promise/park-made :promise/woken :promise/released] (take-last 3 (types)))))

(deftest followup-lifecycle-and-dedupe
  (let [req (merge (dissoc request :awaiting) {:type :inbox-zero :dedupe-key "one" :prompt "test"})
        id (:id (queue/enqueue! req))]
    (queue/enqueue! req)
    (queue/lease-one! "test-p2a" "test-p2a" (constantly true))
    (queue/ack! id)
    (queue/ack! id)
    (is (= [:promise/followup-enqueued :promise/followup-dequeued :promise/followup-terminal] (types)))
    (is (every? #(= :harness (get-in % [:evidence/origin :kind])) (rows)))
    (is (every? #(= "test-p2a" (:evidence/author %)) (rows)))
    (is (= :acked (get-in (last (rows)) [:evidence/body :state])))
    (is (every? #(= id (get-in % [:evidence/body :followup-id])) (rows)))))

(deftest unreachable-real-http-backend-does-not-break-park-or-wake
  ;; A bound, non-listening TCP socket deterministically refuses HTTP. This uses
  ;; the real client/boundary; shorten its restart retry window only for this test.
  (with-open [socket (java.net.Socket.)]
    (.bind socket (java.net.InetSocketAddress. "127.0.0.1" 0))
    (let [port (.getLocalPort socket)]
      (with-redefs [f1b/append-retry-ms 0]
        (binding [history/*backend* (f1b/make-futon1b-backend (str "http://127.0.0.1:" port))]
          (let [failed (:failed (history/stats))
                woke (atom false)
                result (park/park! request {})]
            (is (= :parked (:status result)))
            (park/note-completion! "dep" {:ok true} {:resume! (fn [_] (reset! woke true))})
            (is @woke)
            (is (history/await-writes! 5000))
            (is (= 4 (- (:failed (history/stats)) failed)))
            (is (empty? (:records (park/snapshot))))))))))

(deftest followup-cancel-revalidate-and-expired-lease
  (let [req {:agent "test-p2a" :session "test-p2a" :type :inbox-zero
             :dedupe-key "one" :prompt "test"}
        id (:id (queue/enqueue! req))]
    (queue/cancel! id :operator-released)
    (is (= :operator-released (get-in (last (rows)) [:evidence/body :reason])))
    (queue/enqueue! req)
    (queue/lease-one! "test-p2a" "test-p2a" (constantly :stale))
    (is (= :stale (get-in (last (rows)) [:evidence/body :reason])))
    (queue/enqueue! req)
    (let [leased (queue/lease-one! "test-p2a" "test-p2a" (constantly true))]
      ;; Advance only this persisted lease's clock, no sleeping or fake backend.
      (swap! @#'queue/!state assoc-in [:leased (:followup-id leased) :lease-deadline-ms] 0)
      (queue/lease-one! "test-p2a" "test-p2a" (constantly true))
      (is (= [:promise/followup-requeued :promise/followup-dequeued] (take-last 2 (types)))))))

(deftest each-dependency-is-recorded-and-timer-wakes
  (park/park! (assoc request :awaiting ["a" "b"]) {})
  (park/note-completion! "a" {:ok true} {})
  (park/note-completion! "b" {:ok true} {:resume! (fn [_])})
  (is (= [:promise/park-made :promise/dependency-terminated :promise/dependency-terminated
          :promise/woken :promise/released] (types)))
  (park/park! (assoc request :awaiting [] :timer-due-ms 10) {:now-ms 5})
  (park/sweep-deadlines! {:now-ms 20 :resume! (fn [_])})
  (is (= [:promise/park-made :promise/woken :promise/released] (take-last 3 (types)))))

(deftest outcome-sweep-is-rate-limited
  (let [calls (atom 0)
        last-ms @#'futon3c.agency.promise-history/!outcome-sweep-last-ms
        pending @#'futon3c.agency.promise-history/!outcome-sweep-pending
        saved [@last-ms @pending]]
    (try
      (reset! last-ms 0) (reset! pending false)
      (with-redefs [futon3c.agency.promise-outcome/sweep! (fn [_ _] (swap! calls inc))
                    futon3c.agency.promise-history/backend (fn [] nil)]
        (futon3c.agency.promise-history/sweep-outcomes!)
        (Thread/sleep 300)
        (futon3c.agency.promise-history/sweep-outcomes!)
        (Thread/sleep 300)
        (is (= 1 @calls) "a second sweep inside the interval is skipped")
        (reset! last-ms 0)
        (futon3c.agency.promise-history/sweep-outcomes!)
        (Thread/sleep 300)
        (is (= 2 @calls) "a sweep runs again once the interval has passed"))
      (finally (reset! last-ms (first saved)) (reset! pending (second saved))))))

(deftest outbox-drain-is-idempotent-and-shares-the-chain-allocator
  (let [state (atom {:history-outbox {}})
        persisted (atom nil)
        persist! #(reset! persisted %)
        rec {:id "park:allocator" :agent "agent" :session "session"
             :awaiting #{"job"} :arrived {}}]
    (history/stage! state :promise/park-made rec 1000)
    ;; An old-path transition allocated while sequence 1 is pending must reserve 2.
    (history/record! :promise/deadline-expired rec 2000)
    (history/drain-now! state persist!)
    (history/drain-now! state persist!)
    (is (history/await-writes! 5000))
    (let [entries (->> (rows)
                       (filter #(= "park:allocator"
                                  (get-in % [:evidence/body :history/promise-id])))
                       (sort-by #(get-in % [:evidence/body :history/promise-sequence])))]
      (is (= 2 (count entries)))
      (is (= [1 2] (mapv #(get-in % [:evidence/body :history/promise-sequence]) entries)))
      (is (= 2 (count (set (map :evidence/id entries)))))
      (is (empty? (:history-outbox @state)))
      (is (empty? (:history-outbox @persisted))))))

(deftest failed-authority-persist-enqueues-no-history
  (let [before (count (:order @*evidence*))]
    (with-redefs [atomic-file/write! (fn [& _] (throw (java.io.IOException. "read only")))]
      (is (= :state-persist-failed
             (try (park/park! request {:now-ms 1000}) nil
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
    (is (history/await-writes! 5000))
    (is (= before (count (:order @*evidence*))))))

(deftest chain-check-types-a-durable-pending-predecessor
  (let [pending {:evidence/id "promise-history:pending"
                 :evidence/type :promise/park-made
                 :evidence/body {:history/promise-id "park:pending"
                                 :history/promise-sequence 1}}
        later {:evidence/id "promise-history:later"
               :evidence/type :promise/woken
               :evidence/body {:history/format 3
                               :history/promise-id "park:pending"
                               :history/promise-sequence 2
                               :history/predecessor {:sequence 1
                                                     :id "promise-history:pending"
                                                     :type :promise/park-made}}}]
    (is (= :pending (:reason (first (history/check-chains [later] [pending])))))))
