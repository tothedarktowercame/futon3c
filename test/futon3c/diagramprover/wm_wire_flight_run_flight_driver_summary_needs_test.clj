(ns futon3c.diagramprover.wm-wire-flight-run-flight-driver-summary-needs-test
  "Wire [:flight-run :flight-driver-summary :needs]: the flight's needs
  (run! collects them: the read step's unanswered readings and asks, and
  each click abstention's missing input) reaching the driver's summary —
  run-flight! returns :needs (:needs flown) verbatim.

  No live record carries both ends: the flight records under spike/ carry
  the writer's end (the flight's :needs), but run-flight!'s summary was
  printed to the driver's console output (driver-output-*.txt, not an edn
  record), so the reader's end is on no record (see live-records-read,
  each pinned). So the wire is WITNESSED-HERMETICALLY: run-flight! (the
  reader's var) is called over a temp store with an injected read-text,
  answer-fn and click-fn, which drives flight/run! (the writer's var); the
  mission text has no criteria in a recognised form, so the read step's
  criteria and constraints requests go unanswered and join the flight's
  :needs. The writer's value is (:needs (:flight result)) as run! wrote
  it; the reader's is (:needs result) as run-flight! returned it."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.flight-driver :as fd]
            [futon3c.diagramprover.wm-wire :as w]))

(def mission-text "A mission text with no recognised criteria.\n")

(defn observe
  "run-flight! over a temp store: {:writer the flown flight's :needs,
  :reader the summary's :needs}. CLICK-FN overrides the click (the bad
  cases)."
  ([] (observe nil))
  ([click-fn]
   (let [result (fd/run-flight!
                 (cond-> {:target "M-wire-driver" :repo "futon3c" :path "holes/missions/M-wire.md"
                          :read-text (fn [& _] mission-text)
                          :store (w/tmp-dir "wire-store")
                          :run-record-dir (w/tmp-dir "wire-run-records")
                          :sources {}
                          :answer-fn (fn [_issued] {:state "declined" :seat "wire-seat" :job-id "wire-job-1"})
                          :max-clicks 1
                          :click-fn (fn [_] {:click-id "wire-click-1" :unreached-wants []})}
                   click-fn (assoc :click-fn click-fn))
                 {:flight-id "flight-wire-driver"})]
     {:writer (:needs (:flight result))
      :reader (:needs result)
      :status (:status result)})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-ada87008/flight-ada87008.edn")
      :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
      :why "the writer's end only: the flight record carries :needs ([] — this flight's read step's wants were met by published readings); run-flight!'s summary exists only as console text (driver-output-*.txt), not as a record"}
     {:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "the same: the flight's :needs is on the record, the driver's summary is not a record"}]))

(def wire
  {:wire [:flight-run :flight-driver-summary :needs]
   :kind :witnessed-hermetically
   :test `the-flights-needs-reach-the-drivers-summary
   :check check
   :live-records-read live-records-read})

(deftest the-flights-needs-reach-the-drivers-summary
  (let [o (check)]
    (is (= :closed (:status o)) "the hermetic flight ran to closure")
    (is (= 2 (count (:writer o))) "the unanswered criteria and constraints readings")
    (is (every? #(= :not-answered (:kind %)) (:writer o)))
    (is (= "wire-job-1" (:job-id (first (:writer o)))) "the read step's need, with its job id")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (check)]
    (is (not (w/received? (assoc o :reader {:absent :no-needs}))))
    (is (not (w/received? (assoc o :reader {:status :absent :reason :no-needs}))))))

(deftest a-different-flights-needs-fail-the-wire
  ;; a real call: a click that abstains with a missing input adds a need,
  ;; so the second flight's :needs differ from the first's
  (let [o (check)
        other (observe (fn [_] {:click-id "wire-click-1" :unreached-wants []
                                :abstention {:kind :click-not-started :missing :click
                                             :status 409 :detail {:error "refused"}}}))]
    (is (= 3 (count (:reader other))))
    (is (not= (:writer o) (:reader other)))
    (is (not (w/received? (assoc o :reader (:reader other)))))))

(deftest the-live-records-carry-the-writers-end-only
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (is (contains? (:flight (w/read-record path)) :needs) path)))
