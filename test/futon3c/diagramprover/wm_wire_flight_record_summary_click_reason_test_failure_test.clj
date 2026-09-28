(ns futon3c.diagramprover.wm-wire-flight-record-summary-click-reason-test-failure-test
  "Wire [:flight-record-summary :click-reason-test :failure]: why the
  click closed, as record-summary reads it from the run record's :failure,
  reaching the component's own test
  (futon2/test/futon2/aif/click_reason_test.clj, the :box/kind :test box),
  whose read of this field is

    (is (= (:failure eighth-shaped-record) (:failure e)) \"all four parts\")
    (is (= (:failure e) (flight/click-failure e)))

  in an-eighth-flight-shaped-close-reaches-the-click-entry — e being the
  click entry record-click wrote over record-summary of the run record. A
  test box has no runtime var to drive through, so the hermetic witness
  performs exactly that read over a real call of the writer's var: a judge
  that throws (the box's eighth-flight shape: a substrate-unavailable
  close on a ConnectException) run through
  full-loop-runner/run-opportunity! in hermetic stores, the run record
  read by record-summary, the entry's :failure read beside
  flight/click-failure.

  No live record carries the writer's end: every live run record predates
  WM-CLICK-REASON-I and has no :failure key (see live-records-read, each
  pinned), so the wire is WITNESSED-HERMETICALLY.

  The values are read from the producer record: the producer ran the real
  run-opportunity!, record-summary and record-click; this reader loads no
  product code."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def wire-id [:flight-record-summary :click-reason-test :failure])
(def stem "wm-wire-flight-record-summary-click-reason-test-failure-test")
(def producer (delay (producer-record/record stem)))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn observe [mutation]
  (case mutation
    :none (:primary (fields))
    :different (:different (fields))
    :pinned (:pinned (fields))))
(defn check [] (observe :none))

(def eighth-run-record
  ;; a live run record written before WM-CLICK-REASON-I: no :failure key
  {:path (str w/spike-dir "/flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn")
   :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970"})

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (:path eighth-run-record)
      :sha256 (:sha256 eighth-run-record)
      :why "no :failure key: written before WM-CLICK-REASON-I put the close's failure on the run record"}
     {:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "no :failure key (its one :failure-kind occurrence is a repair finding's, not the run record's)"}
     {:path (p "flight-ada87008/flight-ada87008.edn")
      :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
      :why "the eighth flight's click entry has no :failure — the defect the reader box pins"}]))

(def wire
  {:wire [:flight-record-summary :click-reason-test :failure]
   :kind :witnessed-hermetically
   :test `the-close-failure-reaches-the-components-own-test
   :check check
   :live-records-read live-records-read})

(deftest the-close-failure-reaches-the-components-own-test
  (let [o (check)]
    ;; the reader's own assertions: all five parts, and click-failure agrees
    (is (= {:kind :transport-unavailable :stage :selection
            :error "substrate-2 mission registry unreachable"
            :cause {:cause [{:class "java.net.ConnectException" :message "Connection refused"}]}
            :detail {:absent :no-error-data}}
           (:reader o))
        "[:primary :reader]")
    (is (:reader-agrees? o) "[:primary :reader-agrees?]")
    (is (w/received? o) "[:primary] the writer's value reached the reader")))

(deftest a-record-with-no-failure-is-a-typed-absence-and-fails-the-wire
  ;; the pinned eighth run record through the same vars: the box's own
  ;; a-record-written-before-this-packet case
  (is (= (:sha256 eighth-run-record) (w/sha256-file (:path eighth-run-record))))
  (let [o (observe :pinned)]
    (is (= {:absent :failure-not-on-run-record} (:reader o)) "[:pinned :reader]")
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)}))
        "[:pinned] a typed absence at the reader fails the wire")))

(deftest a-different-failure-fails-the-wire
  (let [o (check)
        other (observe :different)]
    (is (some? (:reader other)) "[:different :reader]")
    (is (not (w/typed-absence? (:reader other))) "[:different :reader] not a typed absence")
    (is (not= (:writer o) (:reader other)) "[:different] not the writer's failure")
    (is (not (w/received? (assoc o :reader (:reader other))))
        "[:different] present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-failure
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (is (not (contains? (w/read-record (:path (first live-records-read))) :failure)))
  (is (not (contains? (w/read-record (:path (second live-records-read))) :failure)))
  (is (not-any? :failure (:clicks (:flight (w/read-record (:path (nth live-records-read 2))))))))
