(ns futon3c.diagramprover.wm-wire-flight-record-summary-flight-record-click-failure-test
  "Wire [:flight-record-summary :flight-record-click :failure]: why the
  click closed, as record-summary reads it from the run record's :failure
  (run-record-failure: the close map's kind/stage/error/cause/detail, each part
  typed absent when the record lacks it, WM-CLICK-REASON-I) reaching the
  flight record's click entry (record-click keeps it: `(contains? click
  :failure) (assoc :failure ...)`).

  No live record carries both ends: every live run record predates
  WM-CLICK-REASON-I and has no :failure key, and every live flight
  record's click entry accordingly carries none (the eighth flight's entry
  said :outcome :incomplete and nothing more — see live-records-read,
  each pinned). So the wire is WITNESSED-HERMETICALLY: a judge that throws
  (a substrate-unavailable close, the click-reason fixture's shape) is run
  through full-loop-runner/run-opportunity! in hermetic stores, and the
  run record it writes — which now carries :failure — is read by
  record-summary (the writer's var) and kept by record-click (the
  reader's var). The values are read from the producer record."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def eighth-run-record
  ;; a live run record written before WM-CLICK-REASON-I: no :failure key
  {:path (str w/spike-dir "/flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn")
   :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970"})

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (:path eighth-run-record)
      :sha256 (:sha256 eighth-run-record)
      :why "no :failure key: written before WM-CLICK-REASON-I put the close's failure on the run record, so the writer's source is absent"}
     {:path (p "flight-ada87008/flight-ada87008.edn")
      :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
      :why "the reader's end absent: the eighth flight's click entry has no :failure — it said :outcome :incomplete and nothing more"}
     {:paths (mapv p ["flight-278b6988.edn" "flight-7f89646a.edn" "flight-e70b4baf.edn"
                      "flight-d00574c8.edn" "flight-ffcd772b.edn" "flight-6cda5ee8.edn"])
      :why "every other flight record: the click entries carry no :failure"}]))

(defn data [] (:fields (producer-record/record "wm-wire-flight-record-summary-flight-record-click-failure-te")))

(defn check [] (:positive (data)))

(def wire
  {:wire [:flight-record-summary :flight-record-click :failure]
   :second-layer {:test `summary-field-is-recorded-without-changing-progress
                  :kind :record :product [:clicks 0 :failure]
                  :intervention :before-reader}
   :kind :witnessed-hermetically
   :test `the-clicks-failure-reaches-the-click-entry
   :check check
   :live-records-read live-records-read})

(deftest the-clicks-failure-reaches-the-click-entry
  (let [o (check)]
    (is (= {:kind :transport-unavailable :stage :selection
            :error "substrate-2 mission registry unreachable"
            :cause {:cause [{:class "java.net.ConnectException" :message "Connection refused"}]}
            :detail {:absent :no-error-data}}
           (:record-failure o))
        "the runner wrote the close's failure onto the run record")
    (is (= (:record-failure o) (:writer o)) "record-summary reads all five parts")
    (is (w/received? o))))

(deftest a-record-with-no-failure-is-a-typed-absence-and-fails-the-wire
  ;; the pinned eighth run record, read through the same vars
  (is (= (:sha256 eighth-run-record) (w/sha256-file (:path eighth-run-record))))
  (let [record (w/read-record (:path eighth-run-record))
        o (:absent (data))]
    (is (not (contains? record :failure)) "the live record predates the field")
    (is (= {:absent :failure-not-on-run-record} (:writer o)))
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-failure-fails-the-wire
  (let [o (check)
        other (:different (data))]
    (is (some? (:reader other)))
    (is (not (w/typed-absence? (:reader other))))
    (is (not= (:writer o) (:reader other)))
    (is (not (w/received? (assoc o :reader (:reader other)))))))

(deftest the-live-records-carry-no-failure
  (doseq [{:keys [path sha256]} (filter :path live-records-read)]
    (is (= sha256 (w/sha256-file path)) path))
  (doseq [path (:paths (nth live-records-read 2))]
    (is (not-any? :failure (:clicks (:flight (w/read-record path)))) path))
  (is (not-any? :failure (:clicks (:flight (w/read-record (:path (second live-records-read))))))))

(deftest summary-field-is-recorded-without-changing-progress
  ;; flight/record-click:426,429 stores the fields; :409-413 determines
  ;; progress from wants/before/after. run!:577 delegates that decision.
  (let [s (:summary (data))]
    (is (:carriers-without-field-equal? s))
    (is (:a-copied? s))
    (is (:b-copied? s))
    (is (:values-differ? s))
    (is (:records-without-field-equal? s))
    (is (= [:no-progress :no-progress] (:statuses s)))
    (is (= [1 1] (:click-counts s)))))
