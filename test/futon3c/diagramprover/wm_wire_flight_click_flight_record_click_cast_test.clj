(ns futon3c.diagramprover.wm-wire-flight-click-flight-record-click-cast-test
  "Wire [:flight-click :flight-record-click :cast]: the cast http-click-fn
  sent the click with (click-cast of the driver's opts, returned as :cast)
  reaching the flight record's click entry (record-click keeps it:
  `(contains? click :cast) (assoc :cast (:cast click))`).

  This wire is VERIFIED on the eighth flight's record
  (flight-ada87008.edn, pinned below): the click entry carries the
  reader's end at [:flight :clicks 0 :cast], and the writer's value is
  recoverable from the same record at [:plan :resolved-steps :cast] —
  resolved-steps's :cast is (click-cast opts) of the same driver flags
  run-flight! handed http-click-fn, so what the plan resolved to send is
  what the click function computed and sent. The two are equal on the
  record.

  No other live flight record carries the field: the seven earlier flights
  predate WM-CAST-I, their click entries have no :cast (the seventh,
  flight-278b6988, is the flight whose missing cast motivated it)."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.flight-runner :as fr]
            [futon3c.diagramprover.wm-wire :as w]))

(def record
  {:path (str w/spike-dir "/flight-ada87008/flight-ada87008.edn")
   :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"})

(def earlier-record
  ;; read only to show the field's absence before WM-CAST-I
  {:path (str w/spike-dir "/flight-278b6988.edn")
   :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"})

(defn check []
  (let [r (w/read-record (:path record))]
    {:writer (get-in r [:plan :resolved-steps :cast])
     :reader (get-in r [:flight :clicks 0 :cast])}))

(def wire
  {:wire [:flight-click :flight-record-click :cast]
   :kind :verified
   :test `the-clicks-cast-reaches-the-flight-records-click-entry
   :check check
   :record record})

(deftest the-clicks-cast-reaches-the-flight-records-click-entry
  (is (= (:sha256 record) (w/sha256-file (:path record))) "the pin is the record read")
  (let [o (check)
        r (w/read-record (:path record))]
    (is (= 1 (count (get-in r [:flight :clicks]))))
    (is (= {:author "claude-6" :reviewer "claude-13"
            :repair-reviewer {:absent :no-repair-reviewer-given}}
           (:writer o))
        "the writer's value: click-cast of the driver's flags, on the plan")
    (is (map? (:reader o)) "the reader's end is the click entry's :cast")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (check)]
    (is (not (w/received? (assoc o :reader {:absent :no-cast}))))
    (is (not (w/received? (assoc o :reader {:status :absent :reason :no-cast}))))))

(deftest a-different-cast-fails-the-wire
  ;; a real call: click-cast of a different seat set (the eighth flight's
  ;; own seats swapped, a repair-reviewer given) is a different cast
  (let [other (fr/click-cast {:author "claude-13" :reviewer "claude-6" :repair-reviewer "kimi-2"})
        o (check)]
    (is (some? other))
    (is (not= (:writer o) other))
    (is (not (w/received? (assoc o :reader other))))))

(deftest the-earlier-flights-carry-no-cast
  (is (= (:sha256 earlier-record) (w/sha256-file (:path earlier-record))))
  (is (not-any? :cast (:clicks (:flight (w/read-record (:path earlier-record)))))))
