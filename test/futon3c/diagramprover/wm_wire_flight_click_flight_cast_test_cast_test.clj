(ns futon3c.diagramprover.wm-wire-flight-click-flight-cast-test-cast-test
  "Wire [:flight-click :flight-cast-test :cast]: the cast http-click-fn
  sent the click with reaching the component's own test
  (futon2/test/futon2/aif/flight_cast_test.clj, the :box/kind :test box),
  whose read of this field is

    (is (= {:author \"a\" :reviewer \"b\"
            :repair-reviewer {:absent :no-repair-reviewer-given}}
           (:cast entry)))

  in the-cast-goes-on-the-body-and-the-click-entry — the entry being the
  click entry record-click wrote over http-click-fn's result in a run! of
  one click. A test box has no runtime var to drive through, so the
  hermetic witness performs exactly that read over a real call of the
  writer's var: http-click-fn with stubbed ports and the pinned seventh
  run record (its read-record!), run! as the test's own `click` fn does,
  then (:cast entry).

  The seat values are the eighth flight's own (author claude-6, reviewer
  claude-13, repair-reviewer not given), so the witnessed value is the
  same map the eighth flight's click entry carries live — the writer's end
  is present live (live-records-read), but the reader is a test, whose
  read is on no record, so the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon3c.diagramprover.wm-wire :as w]))

(def seventh-run-record
  ;; the pinned live run record the reader's own fixture pins (its header:
  ;; spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn)
  {:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
   :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"})

(def eighth-flight-record
  {:path (str w/spike-dir "/flight-ada87008/flight-ada87008.edn")
   :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"})

(def seats {:author "claude-6" :reviewer "claude-13"})

(defn observe
  "http-click-fn (writer) over stubbed ports reading the pinned seventh
  run record, then flight/run! and the reader's read of :cast off the
  click entry. TAMPER edits the click result before record-click (the bad
  cases). {:writer the :cast http-click-fn returned, :reader the entry's
  :cast (a typed absence when the key is not carried)}."
  ([] (observe identity))
  ([tamper]
   (let [run-record (w/read-record (:path seventh-run-record))
         cf (fr/http-click-fn (merge {:today (constantly "2026-09-26")
                                      :post! (constantly {:status 200 :body {:click-id "wm-click-7"}})
                                      :get-status! (constantly {:running? false})
                                      :sleep! (fn [_])
                                      :read-record! (constantly run-record)}
                                     seats))
         result (cf {:flight {:flight/id "flight-278b6988" :target "M-autoclock-in" :click 1}})
         f (flight/run! (flight/start {:target "M-autoclock-in" :chosen-because {:kind :requested}}
                                      {:kind :a-exits :repo "futon3c" :path "p" :read-text (fn [& _] "")}
                                      {:id "flight-278b6988"})
                        {:click-fn (fn [_] (tamper result))
                         :observe-fn (fn [_ _] {}) :sources-fn (constantly {}) :max-clicks 1})
         entry (first (:clicks f))]
     {:writer (:cast result)
      :reader (if (contains? entry :cast) (:cast entry) {:absent :field-not-carried})})))

(defn check [] (observe))

(def live-records-read
  [{:path (:path eighth-flight-record)
    :sha256 (:sha256 eighth-flight-record)
    :why "the writer's end live: the eighth flight's click entry :cast is the value this witness drives; the reader is a test namespace, so its read is on no record"}
   {:path (str w/spike-dir "/flight-278b6988.edn")
    :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
    :why "neither end: the seventh flight predates WM-CAST-I; its click entry carried no cast — the defect the reader box pins"}])

(def wire
  {:wire [:flight-click :flight-cast-test :cast]
   :kind :witnessed-hermetically
   :test `the-cast-reaches-the-components-own-test
   :check check
   :live-records-read live-records-read})

(deftest the-cast-reaches-the-components-own-test
  (is (= (:sha256 seventh-run-record) (w/sha256-file (:path seventh-run-record)))
      "the pin is the run record read")
  (let [o (check)]
    ;; the reader's own assertion, with the eighth flight's seats
    (is (= {:author "claude-6" :reviewer "claude-13"
            :repair-reviewer {:absent :no-repair-reviewer-given}}
           (:reader o)))
    (is (= (get-in (w/read-record (:path eighth-flight-record)) [:flight :clicks 0 :cast])
           (:writer o))
        "the witnessed value is the eighth flight's live cast")
    (is (w/received? o))))

(deftest a-missing-cast-is-a-typed-absence-and-fails-the-wire
  (let [o (observe #(dissoc % :cast))]
    (is (= {:absent :field-not-carried} (:reader o)))
    (is (not (w/received? o))))
  (is (not (w/received? (assoc (check) :reader {:absent :no-cast}))))
  (is (not (w/received? (assoc (check) :reader {:status :absent :reason :no-cast})))))

(deftest a-different-cast-fails-the-wire
  ;; a real call: click-cast of the seats swapped is a different cast
  (let [o (observe #(assoc % :cast (fr/click-cast {:author "claude-13" :reviewer "claude-6"})))]
    (is (= {:author "claude-13" :reviewer "claude-6"
            :repair-reviewer {:absent :no-repair-reviewer-given}}
           (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-are-as-read
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (is (not-any? :cast (:clicks (:flight (w/read-record (:path (second live-records-read))))))))
