(ns futon3c.diagramprover.wm-wire-flight-entry-loop-test-target-source-test
  "Wire [:flight-entry :loop-test :target-source]: how the flight's target
  was placed reaching the loop-closure test.

  The writer is flight-driver/resolve-target (the map's one production
  writer of the flight's placement), which records :target-source :chosen
  when a :chosen-target is given and :target-source :hand-placed otherwise.
  The reader is futon2/test/futon2/aif/loop_closure_test.clj (a :box/kind
  :test box), whose reads of this field are the assertions

    (is (= {:target \"M-autoclock-in\" :target-source :hand-placed}
           (driver/resolve-target {:target \"M-autoclock-in\"})))
    (is (= :hand-placed (:target-source f)))   ; f from #'driver/flight-for

  A test box has no runtime var to drive through, so the hermetic witness
  performs exactly those reads: resolve-target is called (writer), then the
  reader's two read expressions are evaluated and observed, the second off
  a flight built by flight-for as the reader builds it.

  Live records carry the writer's end but cannot carry the reader's: every
  flight record under holes/labs/M-wm-wiring/spike/ records :target-source
  :hand-placed (written by resolve-target through flight-for), but a test
  box's read exists only in a test run, so no record carries both ends and
  the wire is WITNESSED-HERMETICALLY (see live-records-read, each pinned
  and read)."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.flight-driver :as driver]
            [futon3c.diagramprover.wm-wire :as w]))

(def target "M-autoclock-in")

(defn- reader-flight
  "The flight the reader's second read is performed on, built as
  loop_closure_test.clj builds it (flight-for with a stub read-text)."
  []
  (#'driver/flight-for {:target target :repo "futon3c"
                        :path "holes/missions/M-autoclock-in.md"
                        :read-text (fn [& _] "") :id "wire-target-source"}))

(defn observe
  "resolve-target (writer), then the reader's two reads of :target-source as
  loop_closure_test.clj performs them; TAMPER edits the writer's returned
  map before the first read (the bad cases). {:writer the written
  :target-source, :reader the value the reader's first read observes (a
  typed absence when the key is not carried), :reader-flight-read the
  reader's second read, (:target-source f) off flight-for's flight}."
  ([] (observe identity))
  ([tamper]
   (let [w0 (driver/resolve-target {:target target})
         r (tamper w0)
         f (reader-flight)]
     {:writer (:target-source w0)
      :reader (if (contains? r :target-source)
                (:target-source r)
                {:absent :field-not-carried})
      :reader-flight-read (:target-source f)})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "the writer's end twice: [:flight :target-source] :hand-placed and [:plan :placement :target-source] :hand-placed, both written by resolve-target; the reader's end is a test's read, which no record carries"}
     {:path (p "flight-ffcd772b.edn")
      :sha256 "998565fb0a341077ae5b9341d977a9c5bf6db6ea0cf33fead1e00c630f4e2575"
      :why "[:flight :target-source] :hand-placed — the writer's end only, as above"}
     {:path (p "plan-before-run.edn")
      :sha256 "010541c497d4108d0ed6d35dc1d944cbfa83e1f2b6d5c77355f9e624696ca083"
      :why "[:placement :target-source] :hand-placed — the writer's end on the driver's plan; no reader end"}]))

(def wire
  {:wire [:flight-entry :loop-test :target-source]
   :kind :witnessed-hermetically
   :test `the-placement-reaches-the-loop-test
   :check check
   :live-records-read live-records-read})

(deftest the-placement-reaches-the-loop-test
  (let [o (check)]
    (is (= :hand-placed (:writer o)))
    (is (= :hand-placed (:reader-flight-read o))
        "the reader's second read, off flight-for's flight, agrees")
    (is (w/received? o))))

(deftest a-missing-target-source-is-a-typed-absence-and-fails-the-wire
  (let [o (observe #(dissoc % :target-source))]
    (is (= {:absent :field-not-carried} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-placement-fails-the-wire
  (let [o (observe #(assoc % :target-source :chosen))]
    (is (some? (:reader o)))
    (is (not (w/received? o))
        "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-only-the-writers-end
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (let [r (w/read-record (:path (first live-records-read)))]
    (is (= :hand-placed (get-in r [:flight :target-source])))
    (is (= :hand-placed (get-in r [:plan :placement :target-source])))))
