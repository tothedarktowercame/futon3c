(ns futon3c.apm.analyst-campaign-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.analyst-campaign :as sut]
            [futon3c.apm.campaign-machine :as machine])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn addressed [body]
  (assoc body :receipt/id (machine/ledger-digest [body])))

(def close-1
  (addressed {:receipt/type :frame-close :receipt/frame-id "f21"
              :receipt/problem-id "p1" :receipt/input-receipt-ids #{}
              :receipt/trace-id "t1" :receipt/result :closed}))
(def close-2
  (addressed {:receipt/type :frame-close :receipt/frame-id "f22"
              :receipt/problem-id "p2" :receipt/input-receipt-ids #{}
              :receipt/trace-id "t2" :receipt/result :closed}))

(def initial
  (:state (sut/register {:campaign-id "campaign"
                         :analyst-seat "analyst-1"
                         :analyst-card-path "analyst.md"
                         :analyst-card-blob "blob-1"})))

(defn report [frame-number]
  {:analyst-seat "analyst-1"
   :analyst-card {:path "analyst.md" :blob "blob-1"}
   :series-entry {:frame frame-number :score 1}
   :findings [{:kind :transfer-check}]
   :implementation-packets
   [{:packet-id (str "packet-" frame-number)
     :proposed-regime-id (str "future-regime-" frame-number)}]})

(deftest close-wakes-exactly-once-and-proposals-do-not-mutate-frame-state
  (let [wake (sut/wake-after-close initial close-1)
        replay (sut/wake-after-close (:state wake) close-1)
        accepted (sut/accept-analysis (:state wake) (:obligation wake) (report 1))
        after (:state accepted)
        completed-replay (sut/wake-after-close after close-1)]
    (is (= :new (:status wake)))
    (is (= :already-pending (:status replay)))
    (is (:ok accepted))
    (is (= :already-completed (:status completed-replay)))
    (is (= 1 (get-in after [:analyst/tenure :completed])))
    (is (= "future-regime-1"
           (get-in after [:analyst/regime-proposals 0 :proposed-regime-id])))
    (is (nil? (:active/frame after)))))

(deftest second-frame-requires-and-installs-successor
  (let [w1 (sut/wake-after-close initial close-1)
        a1 (sut/accept-analysis (:state w1) (:obligation w1) (report 1))
        w2 (sut/wake-after-close (:state a1) close-2)
        refused (sut/accept-analysis (:state w2) (:obligation w2) (report 2))
        handoff {:successor-seat "analyst-2"
                 :successor-card {:path "analyst-v2.md" :blob "blob-2"}
                 :handoff-receipt-id "handoff-1"}
        accepted (sut/accept-analysis (:state w2) (:obligation w2)
                                      (assoc (report 2) :handoff handoff))]
    (is (some #{:analyst-successor-handoff-required} (:findings refused)))
    (is (:ok accepted))
    (is (:succession? accepted))
    (is (= {:ordinal 2 :completed 0 :limit 2 :seat "analyst-2"
            :card {:path "analyst-v2.md" :blob "blob-2"}}
           (get-in accepted [:state :analyst/tenure])))))

(deftest only-terminal-content-valid-close-can-wake
  (is (:ok (sut/wake-after-close initial
                                 (addressed (assoc (dissoc close-1 :receipt/id)
                                                   :receipt/result :partial)))))
  (is (= :analyst-wake-close-invalid
         (:error/code (sut/wake-after-close initial
                                             (assoc close-1 :receipt/result :void)))))
  (is (= :analyst-wake-close-invalid
         (:error/code (sut/wake-after-close initial
                                             (assoc close-1 :receipt/id "forged"))))))

(deftest accepted-receipt-appends-as-one-parseable-line
  (let [wake (sut/wake-after-close initial close-1)
        accepted (sut/accept-analysis (:state wake) (:obligation wake) (report 1))
        dir (Files/createTempDirectory "analyst-series"
                                       (make-array FileAttribute 0))
        path (.resolve dir "series-inputs.edn")]
    (is (:ok (sut/append-series-input! path (:receipt accepted))))
    (is (= (:receipt accepted)
           (with-open [reader (io/reader (.toFile path))]
             (edn/read-string (first (line-seq reader))))))))
