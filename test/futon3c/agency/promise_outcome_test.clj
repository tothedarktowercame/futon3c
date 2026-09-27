(ns futon3c.agency.promise-outcome-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.promise-outcome :as outcome]
            [futon3c.agency.promise-history :as history]
            [futon3c.agency.promise-replay :as replay]
            [futon3c.agency.parked-on :as park]
            [futon3c.agency.followup-queue :as queue]
            [futon3c.agency.history-constraints :as constraints]))

(def ^:dynamic *store* nil)
(def ^:dynamic *jobs* nil)
(def request {:agent "p5-test" :session "p5-test" :awaiting ["wake-dep"]
              :beneficiary "joe" :deadline "2020-01-01T00:00:00Z"
              :fulfilment-criterion {:kind :job-terminal-ok :job-id "criterion-job"}})
(use-fixtures :each
  (fn [f]
    (let [p (java.io.File/createTempFile "p5-park" ".edn")
          q (java.io.File/createTempFile "p5-followup" ".edn")
          store (atom {:entries {} :order []}) jobs (atom {})]
      (with-redefs-fn {#'park/store-path (constantly (str p))}
        #(binding [*store* store *jobs* jobs history/*backend* store history/*heads* (atom {})
                   queue/*path-override* (str q) outcome/*lookup-job* (fn [id] (get @jobs id))]
           (try (park/clear!) (queue/clear!) (history/await-writes! 5000)
                (reset! store {:entries {} :order []})
                (f)
                (finally (history/await-writes! 5000) (.delete p) (.delete q)
                         (reset! @#'park/!parked nil) (reset! @#'queue/!state nil))))))))
(defn rows [] (is (history/await-writes! 5000)) (vals (:entries @*store*)))
(defn outcomes [] (filter #(outcome/types (:evidence/type %)) (rows)))

(deftest successful-criterion-writes-once-and-keeps-wake-separate
  (let [id (:id (park/park! request {})) wakes (atom 0)]
    (history/await-writes! 5000)
    (swap! *jobs* assoc "criterion-job" {:state "done" :finished-at "2019-12-31T23:59:00Z"})
    (park/note-completion! "wake-dep" {:ok true} {:resume! (fn [_] (swap! wakes inc))})
    (let [r (first (outcomes))]
      (is (= 1 @wakes))
      (is (= 1 (count (outcomes))))
      (is (= :promise/fulfilled (:evidence/type r)))
      (is (= id (get-in r [:evidence/body :promise-id])))
      (is (= "done" (get-in r [:evidence/body :job-observation :state])))
      (is (contains? (:entries @*store*) (get-in r [:evidence/body :source-evidence-id])))
      (is (empty? (:records (park/snapshot))))
      ;; Restart: a fresh backend instance sharing only the persisted entries,
      ;; with no outcome process flags or park cache, must not duplicate the outcome.
      (let [restarted (atom @*store*)]
        (outcome/sweep! restarted (System/currentTimeMillis))
        (outcome/sweep! restarted (System/currentTimeMillis))
        (is (= 1 (count (filter #(outcome/types (:evidence/type %)) (vals (:entries @restarted)))))))
    (is (empty? (:incomplete-promises (constraints/normalize-history (rows)))))
    (is (:equal? (replay/compare-state (rows) {:parked (park/snapshot) :followup (queue/snapshot)}))))))

(deftest bad-case-wake-does-not-fulfil-a-failed-different-criterion
  (let [req (assoc request :deadline "2099-01-01T00:00:00Z")
        id (:id (park/park! req {})) wakes (atom 0)]
    (swap! *jobs* assoc "criterion-job" {:state "failed" :terminal-code "invoke-error"})
    (park/note-completion! "wake-dep" {:ok true} {:resume! (fn [_] (swap! wakes inc))})
    (is (= 1 @wakes))
    (is (empty? (outcomes)))
    (is (some #(= :promise/woken (:evidence/type %)) (rows)))
    (is (empty? (:records (park/snapshot))))
    ;; Sweep after the deadline still finds the released promise in retained history.
    (outcome/sweep! *store* 4102444800001)
    (let [out (outcomes)]
      (is (= [:promise/lapsed] (mapv :evidence/type out)))
      (is (= id (get-in (first out) [:evidence/body :promise-id])))
      (is (= "failed" (get-in (first out) [:evidence/body :job-observation :state]))))))

(deftest prose-and-missing-job-are-never-automatic-outcomes
  (park/park! (assoc request :awaiting [] :fulfilment-criterion {:kind :prose :text "be helpful"})
              {:resume! (fn [_])})
  (park/park! (assoc request :awaiting []) {:resume! (fn [_])})
  (history/await-writes! 5000)
  (outcome/sweep! *store* (System/currentTimeMillis))
  (is (empty? (outcomes))))

(deftest followup-criterion-is-evaluated-and-deadline-sweeps-survive-restart
  (let [req (merge (dissoc request :awaiting) {:type :inbox-zero :dedupe-key "p5" :prompt "test"})
        id (:id (queue/enqueue! req))]
    (history/await-writes! 5000)
    (swap! *jobs* assoc "criterion-job" {:state "failed"})
    (queue/lease-one! "p5-test" "p5-test" (constantly true))
    (queue/ack! id)
    (is (= [:promise/lapsed] (mapv :evidence/type (outcomes))))
    (let [fresh (atom @*store*)]
      (outcome/sweep! fresh (System/currentTimeMillis))
      (is (= 1 (count (filter #(outcome/types (:evidence/type %)) (vals (:entries @fresh)))))))))

(deftest late-success-retains-lapse-and-fulfilment
  (let [r (assoc request :id "test" :fulfilment-criterion
                 {:kind :job-terminal-ok :machine-evaluable? true :job-id "criterion-job"})]
    (is (= [:promise/lapsed :promise/fulfilled]
           (outcome/decide r {:state "done" :finished-at "2020-01-02T00:00:00Z"} 1578009600001)))))
