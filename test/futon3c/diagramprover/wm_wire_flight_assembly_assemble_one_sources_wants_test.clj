(ns futon3c.diagramprover.wm-wire-flight-assembly-assemble-one-sources-wants-test
  "Wire [:tick-flight-assembly :construction-assemble-one [:wants {:record :sources}]]:
  the flight's wants, scoped to the sources record, reaching cascade-problem
  assembly.

  The writer is war-machine/flight-assembly-input: inside a flight it writes
  the sources' wants of the flight's target ((assoc-in [:sources :wants
  target] (vec (:wants flight)))), scoped [:wants {:record :sources}] — a
  different vertex from the click's :wants it reads. The reader is
  cascade-problems/assemble-one, which reads (get-in sources [:wants
  target]) as the problem's :want; a missing or empty want is the typed
  refusal :want-not-declared.

  No live record carries either end: the run records under the spike
  directory carry no assembled cascade problem (no :want) and no sources
  map (see live-records-read). So the wire is WITNESSED-HERMETICALLY:
  flight-assembly-input is called over a fixture flight and sources, and
  its output handed to assemble-one (called at its site var); the writer's
  value is the sources' [:wants target] the writer wrote, the reader's
  value is the assembled problem's :want (the refusal when the read fails)."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.cascade-problems :as cp]
            [futon2.report.war-machine :as wm]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- sources []
  {:universes {} :wants {}
   :interpretations {:T {:patterns {:p {:guard {:needs #{}} :produces #{:a}}}}}
   :locators {:T {:a {:class :C8} :b {:class :C8}}}
   :beta-by-context {:WM {:beta 1.0}}
   :candidates {:T [{:precedence [:p] :construction-receipt {:kind :fixture}}]}})

(def flight {:target :T :wants [:a] :universe {:a false} :locators {}})

(defn observe
  "flight-assembly-input's output through assemble-one for target :T.
  SOURCES-FN rewrites the writer's sources before the reader runs (the bad
  cases). {:writer the sources' [:wants :T], :reader the problem's :want
  (the refusal when assembly refused), :problem the assembly}."
  ([] (observe identity))
  ([sources-fn]
   (let [out (wm/flight-assembly-input flight {:targets [] :sources (sources)})
         srcs (sources-fn (:sources out))
         problem (#'cp/assemble-one srcs 3 :T)]
     {:writer (get-in out [:sources :wants :T])
      :reader (if (:kind problem) problem (get-in problem [:cascade-problem :want]))
      :problem problem})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "carries neither end: no assembled cascade problem (no :cascade-problem :want) and no flight-assembly sources map ([:sources :wants] absent)"}
     {:path (p "tick-run-record-2026-09-26-flight-7f89646a-click-1.edn")
      :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
      :why "the same: no assembled problem, no sources map"}
     {:paths ["holes/labs/M-futon-seams/exemplar/click-001.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-outcome.edn"]
      :why "hand-authored exemplar records; no sources, no assembled problem"}]))

(def wire
  {:wire [:tick-flight-assembly :construction-assemble-one [:wants {:record :sources}]]
   :kind :witnessed-hermetically
   :test `the-flights-wants-reach-assembly
   :check check
   :live-records-read live-records-read})

(deftest the-flights-wants-reach-assembly
  (let [o (check)]
    (is (= [:a] (:writer o)) "the flight's wants written into the sources")
    (is (nil? (get-in o [:problem :kind])) "assembly produced a problem, not a refusal")
    (is (w/received? o) "the problem's :want is the want the writer wrote")))

(deftest a-removed-want-is-a-typed-refusal-and-fails-the-wire
  ;; the writer's entry removed between writer and reader: the reader's
  ;; want check refuses :want-not-declared
  (let [o (observe #(update % :wants dissoc :T))]
    (is (= :want-not-declared (get-in o [:problem :kind])) (pr-str (:problem o)))
    (is (= :wants (:missing (:reader o))))
    (is (not (w/received? o)))))

(deftest a-different-want-fails-the-wire
  ;; a second real flight-assembly-input run (a flight wanting :a and :b):
  ;; present, not absent, but not the writer's [:a]
  (let [flight2 {:target :T :wants [:a :b] :universe {:a false} :locators {}}
        out2 (wm/flight-assembly-input flight2 {:targets [] :sources (sources)})
        problem2 (#'cp/assemble-one (:sources out2) 3 :T)
        o {:writer (:writer (observe))
           :reader (if (:kind problem2) problem2 (get-in problem2 [:cascade-problem :want]))}]
    (is (= [:a :b] (:reader o)) (pr-str problem2))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-neither-end
  (doseq [{:keys [path sha256]} (filter :path live-records-read)]
    (is (= sha256 (w/sha256-file path)) path)))
