(ns futon3c.agency.promise-replay-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.promise-history :as history]
            [futon3c.agency.promise-replay :as replay]
            [futon3c.agency.parked-on :as park]
            [futon3c.agency.followup-queue :as queue]))

(def ^:dynamic *evidence* nil)
(use-fixtures :each
  (fn [f]
    (let [p (java.io.File/createTempFile "replay-park" ".edn")
          q (java.io.File/createTempFile "replay-followup" ".edn")
          evidence (atom {:entries {} :order []})]
      (with-redefs-fn {#'park/store-path (constantly (str p))}
        #(binding [queue/*path-override* (str q) history/*backend* evidence
                   history/*heads* (atom {}) *evidence* evidence]
           (try
             (reset! @#'park/!parked nil) (reset! @#'queue/!state nil)
             (park/clear!) (queue/clear!) (f)
             (finally (history/await-writes! 5000) (.delete p) (.delete q)
                      (reset! @#'park/!parked nil) (reset! @#'queue/!state nil))))))))

(defn rows []
  (is (history/await-writes! 5000))
  (mapv #(get-in @*evidence* [:entries %]) (:order @*evidence*)))
(defn live [] {:parked (park/snapshot) :followup (queue/snapshot)})
(def request {:agent "test" :session "test" :awaiting ["dep"] :payload {:nil nil}
              :beneficiary "joe" :deadline "2026-10-01T00:00:00Z"})
(defn equal! []
  (let [snapshot (live) entries (rows) report (replay/compare-state (reverse entries) snapshot)]
    (is (:equal? report) (pr-str report))
    (is (= snapshot (:states (replay/rebuild (reverse entries)))))
    (is (= snapshot (live)) "Replay never mutates live state")))

(deftest clean-live-snapshot-replay-including-shared-fifos
  (park/park! request {:now-ms 1000})
  (equal!)
  (park/note-completion! "dep" {:ok true}
                         {:now-ms 1000 :resume! #(park/ready-push! "test" "test" (:id %) "prompt")})
  (equal!)
  (let [item (park/ready-lease-one! "test" "test" 2000 100)]
    (equal!)
    (park/ready-ack! (:park-id item)))
  (equal!)
  (let [a (:id (queue/enqueue! {:agent "test" :session "test" :type :inbox-zero
                               :dedupe-key "a" :prompt "A" :metadata {:nil nil}}))]
    (queue/enqueue! {:agent "test" :session "test" :type :inbox-zero :dedupe-key "b" :prompt "B"})
    (equal!)
    (queue/lease-one! "test" "test" (constantly true))
    (equal!)
    (queue/ack! a)
    (equal!)))

(deftest equal-timestamps-use-promise-sequence-and-gap-is-not-equal
  (let [id (:id (park/park! request {:now-ms 1000}))]
    (park/note-completion! "dep" {:ok true} {:now-ms 1000 :resume! (fn [_])})
    (let [entries (rows)
          own (filter #(= id (replay/promise-id %)) entries)
          dropped (first (filter #(= :promise/dependency-terminated (:evidence/type %)) entries))
          cut (remove #(= (:evidence/id dropped) (:evidence/id %)) entries)
          report (replay/compare-state cut (live))]
      (is (= 1 (count (set (map :evidence/at own)))))
      (is (:equal? (replay/compare-state (reverse entries) (live))))
      (is (false? (:equal? report)))
      (is (some #(and (= id (:promise-id %)) (= :missing-transition (:reason %))
                      (= 2 (:sequence %)) (= :promise/dependency-terminated (:predecessor-type %)))
                (:issues report)))
      (is (not-any? (set (map :evidence/id own)) (:replayed (replay/rebuild cut))))
      (println "missing transition:" (pr-str (:issues report))))))

(deftest legacy-and-unrecorded-promises-are-incomplete
  (let [id (:id (park/park! request {}))
        entries (rows)
        legacy (map #(if (= id (replay/promise-id %))
                       (update % :evidence/body dissoc :history/format :history/promise-sequence) %) entries)
        report (replay/compare-state legacy (live))
        absent (replay/compare-state [] (live))]
    (is (false? (:equal? report)))
    (is (some #(= :incomplete-pre-repair-history (:reason %)) (:issues report)))
    (is (nil? (get-in (replay/rebuild legacy) [:states :parked :records id])))
    (is (some #(and (= id (:promise-id %)) (= :no-history (:reason %))) (:issues absent)))
    (is (false? (:equal? absent)))))

(deftest invalid-chain-and-before-values-cannot-silently-pass
  (park/park! request {})
  (let [entries (rows)
        last-row (last entries)
        duplicate (conj entries (assoc last-row :evidence/id "different-id"))
        payload (history/payload last-row)
        changed (assoc-in payload [:changes 0 :edits 0 :absent?] false)
        changed (assoc-in changed [:changes 0 :edits 0 :before] "not-the-recorded-state")
        tampered (conj (vec (butlast entries))
                       (assoc-in last-row [:evidence/body :history/payload-edn] (pr-str changed)))]
    (is (some #(= :duplicate-sequence (:reason %)) (:issues (replay/compare-state duplicate (live)))))
    (is (false? (:equal? (replay/compare-state tampered (live)))))
    (is (some #(= :missing-state-history (:reason %)) (:issues (replay/compare-state tampered (live)))))))

(deftest a-new-snapshot-field-is-a-difference
  (let [report (replay/compare-state (rows) (assoc-in (live) [:parked :dummy] 1))]
    (is (false? (:equal? report)))
    (is (some #(= [:dummy] (:path %)) (:differences report)))))
