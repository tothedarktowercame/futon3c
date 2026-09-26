(ns futon3c.diagramprover.wm-wire-flight-run-flight-driver-summary-readings-test
  "Wire [:flight-run :flight-driver-summary :readings]: the flight's
  readings (run! prepends each read step's return as a :readings entry
  with :before-click) reaching the driver's summary — run-flight! reads
  (:readings flown) and returns it twice removed: :flight verbatim, and
  :readings as a per-question projection ((vec (for [a (:readings flown)
  q (:asked a)] (select-keys q [:kind :want :request-id :seat :job-id
  :outcome])))).

  The projection is never equal to the flight's :readings entry (it is a
  derived shape), so the first layer is observed at the reader's read
  expression — (:readings flown), the value run-flight! read under the
  field, observable as (:readings (:flight result)) — and the projection's
  agreement with the writer's value is asserted beside it (every asked
  question of every reading appears in the summary's :readings). What the
  projection means is the second layer.

  No live record carries both ends: the flight records under spike/ carry
  the writer's end (each has one :readings entry), but run-flight!'s
  summary was printed to the driver's console output
  (driver-output-*.txt, not an edn record), so the reader's end is on no
  record (see live-records-read, each pinned). So the wire is
  WITNESSED-HERMETICALLY: run-flight! (the reader's var) is called over a
  temp store with an injected read-text, answer-fn and click-fn, which
  drives flight/run! (the writer's var); the mission text has no criteria
  in a recognised form, so the read step asks for criteria and constraints
  and the readings entry carries those asks."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.flight-driver :as fd]
            [futon3c.diagramprover.wm-wire :as w]))

(def mission-text "A mission text with no recognised criteria.\n")

(defn observe
  "run-flight! over a temp store: {:writer the flown flight's :readings
  (what run! wrote), :reader the value run-flight! read under :readings
  ((:readings (:flight result)), its read expression at the site),
  :projection the summary's :readings}. TEXT varies the mission text (the
  bad cases)."
  ([] (observe mission-text))
  ([text]
   (let [result (fd/run-flight!
                 {:target "M-wire-driver" :repo "futon3c" :path "holes/missions/M-wire.md"
                  :read-text (fn [& _] text)
                  :store (w/tmp-dir "wire-store")
                  :run-record-dir (w/tmp-dir "wire-run-records")
                  :sources {}
                  :answer-fn (fn [_issued] {:state "declined" :seat "wire-seat" :job-id "wire-job-1"})
                  :max-clicks 1
                  :click-fn (fn [_] {:click-id "wire-click-1" :unreached-wants []})}
                 {:flight-id "flight-wire-driver"})]
     {:writer (:readings (:flight result))
      :reader (:readings (:flight result))
      :projection (:readings result)
      :status (:status result)})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-ada87008/flight-ada87008.edn")
      :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
      :why "the writer's end only: the flight record carries one :readings entry; run-flight!'s summary exists only as console text (driver-output-*.txt), not as a record"}
     {:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "the same: one :readings entry on the flight record; the driver's summary is not a record"}]))

(def wire
  {:wire [:flight-run :flight-driver-summary :readings]
   :kind :witnessed-hermetically
   :test `the-flights-readings-reach-the-drivers-summary
   :check check
   :live-records-read live-records-read})

(deftest the-flights-readings-reach-the-drivers-summary
  (let [o (check)]
    (is (= :closed (:status o)) "the hermetic flight ran to closure")
    (is (= 1 (count (:writer o))) "one read step ran before the click")
    (is (= #{:criteria :constraints} (set (map :kind (:asked (first (:writer o)))))
           (set (map :kind (:projection o))))
        "the summary's :readings is the per-question projection of what run! wrote")
    (is (= 1 (:before-click (first (:writer o)))))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (check)]
    (is (not (w/received? (assoc o :reader {:absent :no-readings}))))
    (is (not (w/received? (assoc o :reader {:status :absent :reason :no-readings}))))))

(deftest a-different-flights-readings-fail-the-wire
  ;; a real call: a mission text with a stated criterion gives the read
  ;; step different work, so the second flight's :readings differ
  (let [o (check)
        other (observe "Exit criteria:\n- [ ] the gate is green\n")]
    (is (some? (:reader other)))
    (is (not= (:writer o) (:reader other)))
    (is (not (w/received? (assoc o :reader (:reader other)))))))

(deftest the-live-records-carry-the-writers-end-only
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (is (= 1 (count (:readings (:flight (w/read-record path))))) path)))
