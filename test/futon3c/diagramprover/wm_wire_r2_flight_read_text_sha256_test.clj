(ns futon3c.diagramprover.wm-wire-r2-flight-read-text-sha256-test
  "Wire [:r2-served-by-reading :r2-flight-read :text-sha256]: the read
  step's text pin reaching the flight record.

  The writer is served-by-reading (futon2.aif.served-by-reading, site var
  `proposal`; the component's :text-sha256 is sha256 of the mission text,
  computed by sbr/sha256 and carried on the served-by reading map). The
  reader is flight-runner/read-fn, whose return map copies the reading's
  pin verbatim (flight_runner.clj: `:text-sha256 (:text-sha256 served-by)`)
  and whose map run! stores as the flight record's :readings entry.

  This wire is VERIFIED: every live flight record under
  holes/labs/M-wm-wiring/spike/ carries both ends of this field in its one
  :readings entry — [:served-by :text-sha256] (the writer's value) and
  :text-sha256 (the reader's copy) — and they are equal. The pin below is
  flight-278b6988.edn, asserted by sha256 before it is read."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]))

(def record
  {:path (str w/spike-dir "/flight-278b6988.edn")
   :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"})

(def other-record
  ;; a second live flight record, read only to supply a different real
  ;; value for the bad case (its reading pinned a different text)
  {:path (str w/spike-dir "/flight-d00574c8.edn")
   :sha256 "68e531b462d535983ea5114a8da370daec943a3b4283b6b87831e63ecf9eadd8"})

(defn check []
  (let [r (w/read-record (:path record))
        rd (first (:readings (:flight r)))]
    {:writer (get-in rd [:served-by :text-sha256])
     :reader (:text-sha256 rd)}))

(def wire
  {:wire [:r2-served-by-reading :r2-flight-read :text-sha256]
   :kind :verified
   :test `the-read-steps-text-pin-reaches-the-flight-record
   :check check
   :record record})

(deftest the-read-steps-text-pin-reaches-the-flight-record
  (is (= (:sha256 record) (w/sha256-file (:path record))) "the pin is the record read")
  (let [o (check)
        rd (first (:readings (:flight (w/read-record (:path record)))))]
    (is (= 1 (count (:readings (:flight (w/read-record (:path record)))))))
    (is (= "98a647bdb1156c2589abc28abbe1dd226aa5a28302cec9afe8e8b9ec80c5b048"
           (:writer o)))
    (is (w/received? o))
    (is (map? (:served-by rd)) "the writer's end is the served-by reading map")))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (check)]
    (is (not (w/received? (assoc o :reader {:absent :no-reading}))))
    (is (not (w/received? (assoc o :reader {:status :absent :reason :no-reading}))))))

(deftest a-different-texts-pin-fails-the-wire
  ;; flight-d00574c8's reading pinned a different mission text; its sha is a
  ;; real value from a live record, and it is not this record's pin
  (is (= (:sha256 other-record) (w/sha256-file (:path other-record))))
  (let [other (get-in (w/read-record (:path other-record)) [:flight :readings 0 :text-sha256])
        o (check)]
    (is (some? other))
    (is (not= (:writer o) other))
    (is (not (w/received? (assoc o :reader other))))))
